#pragma once

#include "battleship.hpp"

#define RAND_PERM(vec, size, rng) std::vector<int> vec(size); \
        std::iota(vec.begin(), vec.end(), 0); \
        std::shuffle(vec.begin(), vec.end(), rng);


inline constexpr uint64_t ship_perm_to_int(const std::vector<int> &perm) {
    uint64_t ret = 0;
    for (size_t i = 0; i < perm.size(); i++)
        ret |= (1 << (i*NUM_SHIPS + perm[i]));
    return ret;
}


// precalculate the restrictions based on misses and ship size.
void BSSampler::create_miss_masks() {
    for (int size = 0; size < NUM_SIZES; size++)
        for (int vert = 0; vert < 2; vert++)
            req_miss_masks[size][vert] = REQ_HIT_MASKS[size][vert][BOARD_SIZE];

    grid_t miss_cpy = misses;
    while (miss_cpy) {
        for (int size = 0; size < NUM_SIZES; size++)
            for (int vert = 0; vert < 2; vert++)
                req_miss_masks[size][vert] &= ~REQ_HIT_MASKS[size][vert][countr_zero(miss_cpy)];
        miss_cpy &= miss_cpy - 1;
    }
}

template <unsigned N, bool ONLY1>
inline bool recurse_randsample(grid_t working, BSSampler &samp, BSConfig2 &conf,
                               int *perm, std::mt19937_64 &rng, uint64_t perm_id) {
    const int ship = perm[N];
    const int ship_size_idx = SHIP_SIZES[ship] - 2;
    grid_t cand_horiz = ~static_cast<grid_t>(0);
    grid_t cand_vert = ~static_cast<grid_t>(0);

    grid_t hit_satis = samp.hits & ~working;
    if (hit_satis)
        cand_vert = cand_horiz = 0;
    while (hit_satis) {
        cand_horiz |= REQ_HIT_MASKS[ship_size_idx][0][countr_zero(hit_satis)];
        cand_vert |= REQ_HIT_MASKS[ship_size_idx][1][countr_zero(hit_satis)];
        hit_satis &= hit_satis - 1;
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
        return false;
    }

//    if (N == 0) {
//        for (int i = 0; i < NUM_SHIPS; i++)
//            std::cout << perm[i] << '\t';
//        dump_board(cand_horiz);
//        dump_board(cand_vert);
//    }

    RAND_PERM(board_seq, tot_cand, rng);

    int size = SHIP_SIZES[ship];
    bool found = false;
    for (int trial : board_seq) {
        bool vert = trial >= nhoriz;
        int sq = countr_zero(isolate_nth_bit(
                vert ? cand_vert : cand_horiz, trial % nhoriz));
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

    return found;
}

template <>
inline bool recurse_randsample<5, true>(grid_t working, BSSampler &samp, BSConfig2 &conf,
                                        int *perm, std::mt19937_64 &rng, uint64_t perm_id) {
    assert(popcnt(working) == 5+4+3+3+2);
    assert(!(working >> (grid_t)BOARD_SIZE));
    assert(!(working & samp.misses));
    if ((working & samp.hits) != samp.hits)
        return false;

    samp.total++;
    while (working) {
        samp.counts[countr_zero(working)]++;
        working &= (working - 1);
    }
    return true;
}

template <>
inline bool recurse_randsample<5, false>(grid_t working, BSSampler &samp, BSConfig2 &conf,
                                         int *perm, std::mt19937_64 &rng, uint64_t perm_id) {
    return recurse_randsample<5, true>(working, samp, conf, perm, rng, perm_id);
}

void BSSampler::try_random(std::mt19937_64 &rng) {
    RAND_PERM(ship_perm, NUM_SHIPS, rng);
    uint64_t perm;
    {
        std::lock_guard<std::mutex> lg(impossible_mtx);
        assert(impossible.size() < 120);
        while (impossible.contains(perm = ship_perm_to_int(ship_perm)))
            std::shuffle(ship_perm.begin(), ship_perm.end(), rng);
    }

    BSConfig2 conf{};
    recurse_randsample<0, true>(0, *this, conf, ship_perm.data(), rng, perm);
}

void BSSampler::multithread_randsample(uint32_t max) {
    unsigned conc = std::thread::hardware_concurrency();
    std::vector<std::thread> threads;
    for (int i = 0; i < conc; i++)
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

    
