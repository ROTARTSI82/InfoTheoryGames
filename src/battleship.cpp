#pragma once

#include "battleship.hpp"

inline constexpr uint64_t ship_perm_to_int(const std::vector<int> &perm) {
    uint64_t ret = 0;
    for (size_t i = 0; i < perm.size(); i++)
        ret |= (static_cast<uint64_t>(1) << (i*NUM_SHIPS + perm[i]));
    return ret;
}


// precalculate the restrictions based on misses and ship size.
void BSSampler::create_miss_masks(std::mt19937_64 &rng, bool use_counts) {
    for (int size = 0; size < NUM_SIZES; size++)
        for (int vert = 0; vert < 2; vert++) {
            req_miss_masks[size][vert] = REQ_HIT_MASKS[size][vert][BOARD_SIZE];

            if (use_counts) {
                grid_t possible = 0;
                for (int sq = 0; sq < BOARD_SIZE; sq++) {
                    if (config_counts[vert * BOARD_SIZE + sq][size] > 0)
                        possible |= mk_mask(sq);
                }

                assert((req_miss_masks[size][vert] & possible) == possible);
                if (possible) {
                    req_miss_masks[size][vert] &= possible;
                    dump_board(possible);
                }
            }
        }

    grid_t miss_cpy = misses;
    while (miss_cpy) {
        for (int size = 0; size < NUM_SIZES; size++)
            for (int vert = 0; vert < 2; vert++)
                req_miss_masks[size][vert] &= ~REQ_HIT_MASKS[size][vert][countr_zero(miss_cpy)];
        miss_cpy &= miss_cpy - 1;
    }

    if (hits) {
        int max = popcnt(hits);
        std::uniform_int_distribution<int> dist(0, max - 1);
        hit_anchor_sq = countr_zero(isolate_nth_bit(hits, dist(rng)));
    } else {
        hit_anchor_sq = -1;
    }
}

template <unsigned N, bool ONLY1>
static inline __attribute__((always_inline))
bool recurse_randsample(grid_t working, BSSampler &samp, BSConfig2 &conf,
                        int *perm, std::mt19937_64 *rng, uint64_t perm_id) {
    const int ship = perm[N];
    const int ship_size_idx = SHIP_SIZES[ship] - 2;
    grid_t cand_horiz = ~static_cast<grid_t>(0);
    grid_t cand_vert = ~static_cast<grid_t>(0);

    grid_t hit_satis = samp.hits & ~working;
    if (N == NUM_SHIPS - 1) {
        // last ship! must get all of them
        while (hit_satis) {
            cand_horiz &= REQ_HIT_MASKS[ship_size_idx][0][countr_zero(hit_satis)];
            cand_vert &= REQ_HIT_MASKS[ship_size_idx][1][countr_zero(hit_satis)];
            hit_satis &= hit_satis - 1;
        }
    } else if (ONLY1) {
        // random sampling: require all ships placed to satisfy a hit until
        // we satisfy all hits.
        // TODO: this is introducing a bias of explaining the seen HITS pattern
        //       with as many ships as possible, leading to extra guessing around hits.
        //       in fact, it's not just this: it's the entire implementation.
        //       the fact that i'm pruning away the impossible configurations as early as possible
        //       as I look means that after a few hits, i am not exploring configuartion
        //       space uniformly at all (it all depends on the order I look in,
        //       and I determine the order by just uniform random at the first level,
        //       which is not correct!). It becomes hopelessly biased. Switch to enumeration!
        if (hit_satis)
            cand_vert = cand_horiz = 0;
        while (hit_satis) {
            cand_horiz |= REQ_HIT_MASKS[ship_size_idx][0][countr_zero(hit_satis)];
            cand_vert |= REQ_HIT_MASKS[ship_size_idx][1][countr_zero(hit_satis)];
            hit_satis &= hit_satis - 1;
        }
    } else if (!ONLY1 && samp.hit_anchor_sq >= 0 && N == 0) {
        // only for enumeration on the first ship placed
        cand_horiz = REQ_HIT_MASKS[ship_size_idx][0][samp.hit_anchor_sq];
        cand_vert = REQ_HIT_MASKS[ship_size_idx][1][samp.hit_anchor_sq];
    }

    cand_vert &= samp.req_miss_masks[ship_size_idx][1];
    cand_horiz &= samp.req_miss_masks[ship_size_idx][0];

    for (unsigned i = 0; i < N; i++) {
        int idx = perm[i];
        int sq = conf.ships[idx];
        idx = SHIP_SIZES[idx] - 2;
        cand_horiz &= REQ_MASKS[idx][sq][ship_size_idx][0];
        cand_vert &= REQ_MASKS[idx][sq][ship_size_idx][1];
    }

    int nhoriz = popcnt(cand_horiz);
    int tot_cand = nhoriz + popcnt(cand_vert);
    if (ONLY1 && tot_cand == 0 && N == 0) {
        std::lock_guard<std::mutex> lg(samp.impossible_mtx);
        samp.impossible.emplace(perm_id);
    }
    if (tot_cand == 0)
        return false;

    std::vector<int> board_seq(tot_cand);
    if (ONLY1) {
        std::iota(board_seq.begin(), board_seq.end(), 0);
        std::shuffle(board_seq.begin(), board_seq.end(), *rng);
    }

    int size = SHIP_SIZES[ship];
    bool found = false;
    for (int i = 0; i < tot_cand; i++) {
        int trial = ONLY1 ? board_seq[i] : i;
        bool vert = trial >= nhoriz;
        int sq;

        if (ONLY1) {
            sq = countr_zero(isolate_nth_bit(
                    vert ? cand_vert : cand_horiz, trial % nhoriz));
        } else {
            grid_t &from = cand_horiz ? cand_horiz : cand_vert;
            sq = countr_zero(from);
            from &= from - 1;
        }

        int x = sq % BOARD_WIDTH;
        int y = sq / BOARD_WIDTH;

        grid_t mask = mk_ship_mask(size, vert, x, y);

        assert(!(mask >> static_cast<grid_t>(BOARD_SIZE)));
        assert(popcnt(mask) == size);
        assert(popcnt(working | mask) == popcnt(working) + popcnt(mask));
        conf.ships[ship] = (vert?BOARD_SIZE:0) + y*BOARD_WIDTH + x;
        found |= recurse_randsample<N+1, ONLY1>(working | mask, samp, conf, perm, rng, perm_id);
        if (found && ONLY1)
            return true;
    }

    if (ONLY1 && N == 0) {
        std::lock_guard<std::mutex> lg(samp.impossible_mtx);
        samp.impossible.emplace(perm_id);
    }

    if (!ONLY1 && N < 3) {
        std::cout << "tot = " << samp.total.load(std::memory_order_relaxed) << '\n';
    }

    return found;
}

template <>
inline __attribute__((always_inline))
bool recurse_randsample<5, true>(grid_t working, BSSampler &samp, BSConfig2 &conf,
                                 int *perm, std::mt19937_64 *rng, uint64_t perm_id) {
    assert(popcnt(working) == 5+4+3+3+2);
    assert(!(working >> (grid_t)BOARD_SIZE));
    assert(!(working & samp.misses));
    assert((working & samp.hits) == samp.hits);

    for (int i = 0; i < NUM_SHIPS; i++) {
        int size_idx = SHIP_SIZES[i] - 2;
        samp.config_counts[conf.ships[i]][size_idx].fetch_add(1, std::memory_order_relaxed);
    }

    samp.total.fetch_add(1, std::memory_order::relaxed);
    return true;
}

template <>
inline __attribute__((always_inline))
bool recurse_randsample<5, false>(grid_t working, BSSampler &samp, BSConfig2 &conf,
                                  int *perm, std::mt19937_64 *rng, uint64_t perm_id) {
    return recurse_randsample<5, true>(working, samp, conf, perm, rng, perm_id);
}

void BSSampler::try_random(std::mt19937_64 &rng) {
    std::vector<int> ship_perm(NUM_SHIPS);
    std::iota(ship_perm.begin(), ship_perm.end(), 0);
    std::shuffle(ship_perm.begin(), ship_perm.end(), rng);
    uint64_t perm;
    {
        std::lock_guard<std::mutex> lg(impossible_mtx);
        assert(impossible.size() < 120);
        while (impossible.contains(perm = ship_perm_to_int(ship_perm)))
            std::shuffle(ship_perm.begin(), ship_perm.end(), rng);
    }

    BSConfig2 conf{};
    recurse_randsample<0, true>(0, *this, conf, ship_perm.data(), &rng, perm);
}

void BSSampler::enumerate() {
    BSConfig2 conf{};
    int perm[] = {0, 1, 2, 3, 4};
    recurse_randsample<0, false>(0, *this, conf, perm, nullptr, 0);
}

void BSSampler::multithread_enum() {

}

void BSSampler::multithread_randsample(uint32_t max) {
    unsigned conc = std::thread::hardware_concurrency();
    std::vector<std::thread> threads;
    threads.reserve(conc);
    for (unsigned i = 0; i < conc; i++)
        threads.emplace_back([=, this]() {
            std::random_device dev;
            std::mt19937_64 rng(dev());
            while (total < max) {
                try_random(rng);
                its++;
                if (its % 4096 == 0)
                    std::cout << "prog = " << total << " / " << its << std::endl;
            }
        });

    for (auto &t : threads)
        t.join();
}

void BSSampler::config_to_probs() {
    for (double &prob : probs)
        prob = 0;

    for (int sq = 0; sq < BOARD_SIZE; sq++)
        for (int vert = 0; vert < 2; vert++)
            for (int size = 0; size < NUM_SIZES; size++) {
                uint64_t inc = config_counts[sq + vert * BOARD_SIZE][size];
                if (!inc)
                    continue;
                grid_t mask = mk_ship_mask(2+size, vert, sq);
//                dump_board(mask, true);
                while (mask) {
                    probs[countr_zero(mask)] += (double) inc;
                    mask &= mask - 1;
                }
            }

    max_prob = 0;
    next_guess_sq = -1;
    sumprobs = 0;
    for (int sq = 0; sq < BOARD_SIZE; sq++) {
        probs[sq] /= (double) total;
        sumprobs += probs[sq];
        if (probs[sq] > max_prob && !((hits | misses) & mk_mask(sq))) {
            max_prob = probs[sq];
            next_guess_sq = sq;
        }
    }
}
