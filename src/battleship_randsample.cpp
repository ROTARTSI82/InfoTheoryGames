#pragma once

#include "battleship.hpp"

#define RAND_PERM(vec, size, rng) std::vector<int> vec(size); \
        std::iota(vec.begin(), vec.end(), 0); \
        std::shuffle(vec.begin(), vec.end(), rng);


inline uint64_t ship_perm_to_int(const std::vector<int> &perm) {
    uint64_t ret = 0;
    for (size_t i = 0; i < perm.size(); i++)
        ret |= (1 << (i*NUM_SHIPS + perm[i]));
    return ret;
}

struct BSRandSample {
    grid_t hits;
    grid_t misses;

    std::atomic_uint32_t counts[BOARD_SIZE];
    std::atomic_uint32_t total;

    std::unordered_set<u128> found;
    std::unordered_set<uint64_t> impossible;

    void clear() {
        found.clear();
        impossible.clear();
        total = 0;
        for (int x = 0; x < BOARD_SIZE; x++)
            counts[x] = 0;
    }

    // returns 1 if the permuation found something
    // 0 if it was impossible.
    // 2 if we found a duplicate.
    // 3 if we failed to satisfy all hits or it was impossible
    int try_random(std::mt19937_64 &rng) {
        RAND_PERM(ship_perm, NUM_SHIPS, rng);

        int perm;
        while (impossible.contains(perm = ship_perm_to_int(ship_perm)))
            std::shuffle(ship_perm.begin(), ship_perm.end(), rng);

        grid_t working = 0;
        BSConfig conf{};
        for (int ship : ship_perm) {
            RAND_PERM(board_seq, BOARD_SIZE*2, rng);

            int size = SHIP_SIZES[ship];
            for (int trial : board_seq) {
                bool vert = trial >= BOARD_SIZE;
                int sq = trial % BOARD_SIZE;
                int x = sq % BOARD_WIDTH;
                int y = sq / BOARD_WIDTH;

                if ((!vert && x + size > BOARD_WIDTH)
                    || (vert && y + size > BOARD_HEIGHT)) // off board 
                    continue;
                grid_t mask = shift2d((vert ? VMASKS : HMASKS)[size - 2], x, y);
                if (working & mask || misses & mask)
                    continue; // blocked by another ship or "miss"
                if (ship == ship_perm.back() && ((~(working | mask)) & hits)) {
                    std::cout << "fail\n";
                    continue; // last ship & we havent satisfied all hits
                }
                // if (ship == ship_perm.front() && ((mask & hits) == 0)) 
                //     continue; // first ship does not satisfy any hits.
                //               // idk if we actually want to skip these.
                working |= mask;
                conf.set_ship_vert(ship, vert);
                conf.ships[ship].r = y;
                conf.ships[ship].c = x;
                goto succ;
            }

            if (ship == ship_perm.back())
                return 3;

            impossible.emplace(perm);
            return 0; // impossible

        succ:
            ;
        }

        // there is currently a bug, this assert triggered once.
        if (popcnt(working) != 5+4+3+3+2) {
            dump_board(working);
            assert(false);
        }
        if (found.emplace(conf.to_bytes()).second) {
            total++;
            for (int i = 0; i < BOARD_SIZE; i++)
                if (working & mk_mask(i))
                    counts[i]++;
            return 1;
        }

        return 2;
    }

    
};
