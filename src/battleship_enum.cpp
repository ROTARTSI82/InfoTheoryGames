#pragma once



#include "battleship.hpp"

// todo: generalize special treatment of 1st to
// enable using up to 5 anchor "hit" points.

// possible bug: if ships are touching,
// we only count the bitboards and not the individual arrangements
// that lead to the overall occupancy state.

/*
                2, 4    1854594
                2, 5    1876025
                2, 6    1897168
                2, 7    1918313
                2, 8    1939601
                2, 9    1950318
                3, 0    1961469
                3, 1    1961469
                3, 2    1983334
                3, 3    2004765
                3, 4    2025909
                3, 5    2047047
*/



// for 9x9 with 4 ships (error of 54322)
/* 
states: 78801260
lf buckets=576460752303423487, load=1, size=576460752303423487, buckets=105359939
*/

struct StateEnumerator {
    std::unordered_set<grid_t> gamestates;
    std::mutex mtx;

    // read only after creating the struct.
    grid_t hits = 0;
    grid_t misses = 0;
    grid_t hit_anchors[NUM_SHIPS-1];

    // populate hit_anchors with random hits from `hits`.
    void random_populate_hit_anchors(std::mt19937_64 &rng) {
        int sample_space = popcnt(hits);
        std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,sample_space-1);

        for (int i = 0; i < NUM_SHIPS - 1; i++)
            hit_anchors[i] = isolate_nth_bit(hits, dist(rng));
    }

    void launch_multithread() {
        std::vector<std::thread> threads;
        int conc = std::thread::hardware_concurrency();
        std::cout << "launching " << conc << " threads\n";
        int num = BOARD_SIZE / conc;

        for (int t = 0; t < conc; t++) {
            threads.emplace_back([=, this]() {
                for (int p = num * t; 
                    p < (t == conc-1 ? BOARD_SIZE : num * (t+1));
                    p++) {
                    int x = p % BOARD_WIDTH;
                    int y = p / BOARD_WIDTH;

                    if (hit_anchors[0])
                        for (int ship = 0; ship < NUM_SHIPS; ship++)
                            recurse_enum(0, ship, ship, x, y);
                    else
                        recurse_enum(0, 0, 0, x, y);
                }
            });
        }

        int i = 0;
        for (auto &t : threads) {
            std::cout << "joining " << i++ << '\n';
            t.join();
        }
        return;
    }

    /**
     * enumerate all possible ways of placing the remaining ships
     * onto the board state given by `working`. `ship_no` is the next
     * ship out of the 5 to place, and `excl` is the ship we are excluding
     * (or set to a value outside of [0,4] if we want to place all 5 ships).
     * 
     * `excl` is used when we want to enumerate the ship type we place first,
     * as a good strat is to satisfy the hit marker given by `hit_anchor`
     * and then go on, to eliminate many branches as early as possible.
     */
    inline void enumerate(grid_t working, int ship_no, int excl) {
        // assert(!(working >> (grid_t)100));
        if (ship_no >= NUM_SHIPS) {
            if ((~working) & hits)
                return; // there is a 0 in `working` where there should be a hit.
            std::lock_guard<std::mutex> lg(mtx);
            gamestates.emplace(working);
            return;
        }

        // int sum = 0;
        // for (int i = 0; i < ship_no; i++)
        //     sum += SHIP_SIZES[i];
        // assert(popcnt(working) == sum);

        for (int x = 0; x < BOARD_WIDTH; x++) {
            for (int y = 0; y < BOARD_HEIGHT; y++) {
                if (ship_no < 2) {
                    for (int i = 0; i < ship_no; i++) std::cout << '\t';
                    std::cout << x << ", " << y << '\t' << std::this_thread::get_id() << '\t' << gamestates.size() << '\n';
                }

                recurse_enum(working, ship_no, excl, x, y);
            }
        }
    }

    inline void recurse_enum(grid_t working, int ship_no, int excl, int x, int y) {
        grid_t mask;
        const int size = SHIP_SIZES[ship_no];

        int next = ship_no + 1 == excl          ? ship_no + 2 :
                   ship_no == excl && excl != 0 ? 0
                   : ship_no + 1;
        
        // if (x == 0 && y == 0)
        //     std::cout << "\texcl = " << excl << "\tship_no = " << ship_no << '\n';

        // first handle vertical placement, then horizontal.
        if (x + size > BOARD_WIDTH) // off board 
            goto skip_vert;
        mask = shift2d(VMASKS[size - 2], x, y);
        if (mask & working // blocked by another ship
            || mask & misses // blocked by "miss" marker
            || (ship_no == excl && (~mask) & hit_anchors[0])) // anchor 1st ship to a hit
            goto skip_vert;
        enumerate(working | mask, next, excl);

    skip_vert:
        if (y + size > BOARD_HEIGHT)
            goto skip_horiz;
        mask = shift2d(HMASKS[size - 2], x, y);
        if (mask & working
            || mask & misses
            || (ship_no == excl && (~mask) & hit_anchors[0]))
            goto skip_horiz;
        enumerate(working | mask, next, excl);
    skip_horiz:
        ;
    }
};


