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


// copied from battleship_enum.
void BSSampler::random_populate_hit_anchors(std::mt19937_64 &rng) {
    int sample_space = popcnt(hits);
    std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,sample_space-1);
    for (int i = 0; i < NUM_SHIPS - 1; i++)
        hit_anchors[i] = 0;

    for (int i = 0; i < NUM_SHIPS - 1; i++) {
        if (i >= sample_space)
            break;
        grid_t prev = i > 0 ? hit_anchors[i-1] : 0;
        hit_anchors[i] = prev;
        grid_t cand = 0;
        while ((cand & prev) == cand)
            cand = isolate_nth_bit(hits, dist(rng));
        hit_anchors[i] |= cand;

        std::cout << "hit anchor " << i << '\n';
        dump_board(hit_anchors[i], true);
    }
}

// returns 1 if the permuation found something
// 0 if it was impossible.
// 2 if we found a duplicate.
// 3 if we failed to satisfy all hits or it was impossible
int BSSampler::try_random(std::mt19937_64 &rng) {
    RAND_PERM(ship_perm, NUM_SHIPS, rng);
    uint64_t perm;
    {
        std::lock_guard<std::mutex> lg(impossible_mtx);
        assert(impossible.size() < 120);
        while (impossible.contains(perm = ship_perm_to_int(ship_perm)))
            std::shuffle(ship_perm.begin(), ship_perm.end(), rng);
    }

    grid_t working = 0, mask = 0;
    BSConfig2 conf{};
    int ship_idx = 0;
    for (int ship : ship_perm) {
        assert(!(working >> (grid_t)100));

        const int ship_size_idx = SHIP_SIZES[ship] - 2;
        grid_t cand_horiz = REQ_HIT_MASKS[ship_size_idx][0][BOARD_SIZE];
        grid_t cand_vert = REQ_HIT_MASKS[ship_size_idx][1][BOARD_SIZE];

        grid_t miss_cpy = misses;
        while (miss_cpy) {
            cand_horiz &= ~REQ_HIT_MASKS[ship_size_idx][0][countr_zero(miss_cpy)];
            cand_vert &= ~REQ_HIT_MASKS[ship_size_idx][1][countr_zero(miss_cpy)];
            miss_cpy &= miss_cpy - 1;
        }

        // todo: upgraded hit anchors is a possibility.
        // grid_t hit_cpy = hits;

        for (int i = 0; i < ship_idx; i++) {
            int idx = ship_perm[i];
            int sq = conf.ships[idx];
            idx = SHIP_SIZES[idx] - 2;
            cand_horiz &= REQ_MASKS[idx][sq][ship_size_idx][0];
            cand_vert &= REQ_MASKS[idx][sq][ship_size_idx][1];
        }

        int nhoriz = popcnt(cand_horiz);

        RAND_PERM(board_seq, nhoriz + popcnt(cand_vert), rng);

        int size = SHIP_SIZES[ship];
        for (int trial : board_seq) {
            bool vert = trial >= nhoriz;
            int sq = countr_zero(isolate_nth_bit(
                vert ? cand_vert : cand_horiz, trial % nhoriz));
            int x = sq % BOARD_WIDTH;
            int y = sq / BOARD_WIDTH;

            mask = mk_ship_mask(size, vert, x, y);
            if (misses & mask)
                continue; // blocked by a "miss"
            if (ship == ship_perm.back()) {
                if (((working | mask) & hits) != hits)
                    continue; // we placed the last ship without satisfying all hits
            } else if (((working | mask) & hit_anchors[ship_idx]) != hit_anchors[ship_idx])
                continue; // ship does not satisfy required hit anchor

            assert(!(mask >> static_cast<grid_t>(BOARD_SIZE)));
            assert(popcnt(mask) == size);
            assert(popcnt(working | mask) == popcnt(working) + popcnt(mask));
            working |= mask;
            conf.ships[ship] = (vert?BOARD_SIZE:0) + y*BOARD_WIDTH + x;
            goto succ;
        }

        if (ship == ship_perm.back())
            return 3;

        impossible_mtx.lock();
        impossible.emplace(perm);
        impossible_mtx.unlock();
        return 0; // impossible

    succ:
        ship_idx++;
    }

    assert(popcnt(working) == 5+4+3+3+2);
    assert(!(working >> (grid_t)BOARD_SIZE));

    total++;
    while (working) {
        counts[countr_zero(working)]++;
        working &= (working - 1);
    }
    return 1;
}

void BSSampler::multithread_randsample(uint32_t max) {
    int conc = std::thread::hardware_concurrency();
    std::vector<std::thread> threads;
    for (int i = 0; i < conc; i++)
        threads.emplace_back([=, this]() {
            std::random_device dev;
            std::mt19937_64 rng(dev());
            while (total < max) {
                try_random(rng);
                its++;
                if (its % (4096*8) == 0)
                    std::cout << "prog = " << total << " / " << its << std::endl;
            }
        });

    for (auto &t : threads)
        t.join();
}

    
