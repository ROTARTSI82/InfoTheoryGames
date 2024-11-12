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



// bit 0 - skip horizontal
// bit 1 - skip vertical
// bit 2 - enable extra checks
template <uint8_t FLAGS>
inline void recurse_enum(BSSampler *sampler, BSConfig2 conf, grid_t working, int ship_no, int excl, int x, int y) {
    grid_t mask;
    const int size = SHIP_SIZES[ship_no];

    int next = ship_no + 1 == excl          ? ship_no + 2 :
               ship_no == excl && excl != 0 ? 0
                                            : ship_no + 1;

    // if (x == 0 && y == 0)
    //     std::cout << "\texcl = " << excl << "\tship_no = " << ship_no << '\n';

    // first handle vertical placement, then horizontal.
    if (FLAGS & 1 || (FLAGS & 4 && x + size > BOARD_WIDTH)) // off board
        goto skip_horiz;
    mask = shift2d(HMASKS[size - 2], x, y);
    if (FLAGS & 4 && (mask & sampler->misses // blocked by "miss" marker
                      || (ship_no == excl && (mask & sampler->hit_anchors[0]) != sampler->hit_anchors[0]))) // anchor 1st ship to a hit
        goto skip_horiz;
    conf.ships[ship_no] = y * BOARD_WIDTH + x;
    sampler->enumerate(working | mask, conf, next, excl);

    skip_horiz:
    if (FLAGS & 2 || (FLAGS & 4 && y + size > BOARD_HEIGHT))
        goto skip_vert;
    mask = shift2d(VMASKS[size - 2], x, y);
    if (FLAGS & 4 && (mask & sampler->misses
                      || (ship_no == excl && (mask & sampler->hit_anchors[0]) != sampler->hit_anchors[0])))
        goto skip_vert;
    conf.ships[ship_no] = 100 + y * BOARD_WIDTH + x;
    sampler->enumerate(working | mask, conf, next, excl);
    skip_vert:
    ;
}


void BSSampler::multithread_enum() {
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
                        recurse_enum<4>(this, BSConfig2{}, 0, ship, ship, x, y);
                else
                    recurse_enum<4>(this, BSConfig2{}, 0, 0, -1, x, y);
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
void BSSampler::enumerate(grid_t working, BSConfig2 conf, int ship_no, int excl) {
    assert(!(working >> (grid_t)BOARD_SIZE));

    if (ship_no >= NUM_SHIPS) {
        assert(popcnt(working) == 5+4+3+3+2);
        if ((~working) & hits)
            return; // there is a 0 in `working` where there should be a hit.
        // std::lock_guard<std::mutex> lg(mtx);
        total++;
        // for (int x = 0; x < NUM_SHIPS; x++)
        //     grid[x][conf.ships[x]]++;
        for (int i = 0; i < 5+4+3+3+2; i++) {
            counts[countr_zero(working)]++;
            working &= working - 1;
        }
        return;
    }

    grid_t cand_horiz = REQ_MASKS[ship_no][0][ship_no][0];
    grid_t cand_vert = REQ_MASKS[ship_no][0][ship_no][1];
    // REQ_MASKS[NUM_SHIPS][BOARD_SIZE*2][NUM_SHIPS][2];

    if (ship_no != excl) {
        for (int i = 0; i < ship_no; i++) {
            int sq = conf.ships[i];
            cand_horiz &= REQ_MASKS[i][sq][ship_no][0];
            cand_vert &= REQ_MASKS[i][sq][ship_no][1];
        }
        if (excl >= 0) {
            int sq = conf.ships[excl];
            cand_horiz &= REQ_MASKS[excl][sq][ship_no][0];
            cand_vert &= REQ_MASKS[excl][sq][ship_no][1];
        }
    }

    // dump_board(cand_vert);
    while (cand_vert) {
        int sq = countr_zero(cand_vert);
        recurse_enum<1>(this, conf, working, ship_no, excl, sq % BOARD_WIDTH, sq / BOARD_WIDTH);
        cand_vert &= cand_vert - 1;
    }

    while (cand_horiz) {
        int sq = countr_zero(cand_horiz);
        recurse_enum<2>(this, conf, working, ship_no, excl, sq % BOARD_WIDTH, sq / BOARD_WIDTH);
        cand_horiz &= cand_horiz - 1;
    }

    if (ship_no < 2 || ship_no == excl) {
        for (int i = 0; i < ship_no; i++) std::cout << '\t';
        std::cout << std::this_thread::get_id() << '\t' << total << std::endl;
    }
}


