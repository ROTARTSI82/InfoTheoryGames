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
    grid_t hits = 0;
    grid_t misses = 0;
    grid_t hit_anchors[4] = {0};

    std::atomic_uint32_t counts[BOARD_SIZE];
    std::atomic_uint32_t total;
    std::atomic_uint32_t its;

    // std::mutex mtx;
    // std::unordered_set<uint64_t> found;

    std::mutex impossible_mtx;
    std::unordered_set<uint64_t> impossible;

    void clear() {
        // found.clear();
        impossible.clear();
        total = 0;
        its = 0;
        for (int x = 0; x < BOARD_SIZE; x++)
            counts[x] = 0;
    }

    // copied from battleship_enum.
    void random_populate_hit_anchors(std::mt19937_64 &rng) {
        int sample_space = popcnt(hits);
        std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,sample_space-1);

        for (int i = 0; i < NUM_SHIPS - 1; i++) {
            grid_t prev = i > 0 ? hit_anchors[i-1] : 0;
            hit_anchors[i] = prev;
            grid_t cand = 0;
            while ((cand & prev) == cand)
                cand = isolate_nth_bit(hits, dist(rng));
            hit_anchors[i] |= cand;
        }
    }

    // returns 1 if the permuation found something
    // 0 if it was impossible.
    // 2 if we found a duplicate.
    // 3 if we failed to satisfy all hits or it was impossible
    int try_random(std::mt19937_64 &rng) {
        RAND_PERM(ship_perm, NUM_SHIPS, rng);
        int perm;
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

            grid_t cand_horiz = REQ_MASKS[ship][0][ship][0] & ~misses; 
            grid_t cand_vert = REQ_MASKS[ship][0][ship][1] & ~misses;
            // REQ_MASKS[NUM_SHIPS][BOARD_SIZE*2][NUM_SHIPS][2];
            for (int i = 0; i < ship_idx; i++) {
                int idx = ship_perm[i];
                int sq = conf.ships[idx];
                cand_horiz &= REQ_MASKS[idx][sq][ship][0];
                cand_vert &= REQ_MASKS[idx][sq][ship][1];
            }

            int nhoriz = popcnt(cand_horiz);

            RAND_PERM(board_seq, nhoriz + popcnt(cand_vert), rng);
            // std::cout << std::endl << "size = " << board_seq.size() << std::endl;
            // dump_board(cand_vert, true);
            // dump_board(cand_horiz, true);

            int size = SHIP_SIZES[ship];
            for (int trial : board_seq) {
                bool vert = trial >= nhoriz;
                int sq = countr_zero(isolate_nth_bit(
                    vert ? cand_vert : cand_horiz, trial % nhoriz));
                int x = sq % BOARD_WIDTH;
                int y = sq / BOARD_WIDTH;

                // if (((!vert) && x + size > BOARD_WIDTH)
                //     || (vert && y + size > BOARD_HEIGHT)) {// off board 
                // debug_dump_and_abort:
                //     std::cout << "ship size = " << size << '\n';
                //     dump_board(cand_vert, true);
                //     std::cout << "vertical\n";
                //     dump_board(cand_horiz, true);
                //     std::cout << "working\n";
                //     dump_board(working, true);
                //     assert(false);
                //     continue;
                // }

                mask = mk_ship_mask(size, vert, x, y);

                // if (working & mask)
                //     goto debug_dump_and_abort;
                if (misses & mask)
                    continue; // blocked by a "miss"
                if (ship == ship_perm.back()) {
                    if ((~(working | mask)) & hits)
                        continue; // we placed the last ship without satisfying all hits
                } else if (((working | mask) & hit_anchors[ship_idx]) != hit_anchors[ship_idx]) 
                    continue; // ship does not satisfy required hit anchor

                assert(!(mask >> static_cast<grid_t>(BOARD_SIZE)));
                assert(popcnt(mask) == size);
                assert(popcnt(working | mask) == popcnt(working) + popcnt(mask));
                working |= mask;
                conf.ships[ship] = (vert?BOARD_SIZE:0) + y*BOARD_WIDTH + x;
                // conf.set_ship_vert(ship, vert);
                // conf.ships[ship].r = y;
                // conf.ships[ship].c = x;
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

        // std::lock_guard<std::mutex> lg(mtx);
        // if (found.insert(conf.to_bytes()).second) {
        //     total++;
        //     while (working) {
        //         counts[countr_zero(working)]++;
        //         working &= (working - 1);
        //     }
        //     return 1;
        // }

        // return 2;
    }

    void launch_multithread(uint32_t max) {
        int conc = std::thread::hardware_concurrency();
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

    
};
