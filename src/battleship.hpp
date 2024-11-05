#pragma once

#ifndef BATTLESHIP_HPP
#define BATTLESHIP_HPP

#include <iostream>
#include <unordered_set>
#include <cstdint>
#include <cassert>
#include <thread>
#include <bit>
#include <random>
#include <algorithm>

using grid_t = __uint128_t;
using u128 = grid_t;

constexpr int SHIP_SIZES[] = {5,4,3,3,2};
constexpr int NUM_SHIPS = 5;
constexpr int BOARD_WIDTH = 10;
constexpr int BOARD_HEIGHT = 10;
constexpr int BOARD_SIZE = BOARD_WIDTH * BOARD_HEIGHT;

inline constexpr grid_t mk_mask(int n) {
    return static_cast<grid_t>(1) << static_cast<grid_t>(n);
}

inline constexpr grid_t mk_mask(int x, int y) {
    return mk_mask(y * BOARD_WIDTH + x);
}

template <bool VERT>
inline constexpr grid_t lmask(int n) {
    grid_t ret = 0;
    for (int i = 0; i < n; i++)
        ret |= VERT ? mk_mask(0,i) : mk_mask(i);
    return ret;
}

void dump_board(grid_t grid, bool full = false) {
    for (int y = 0; y < (full ? 13 : BOARD_HEIGHT); y++) {
        std::cout << "\n" << y << "\t";
        for (int x = 0; x < BOARD_WIDTH; x++)
            std::cout << ((grid & mk_mask(x, y)) ? 'x' : '.');
    }
    std::cout << '\n';
}

constexpr grid_t VMASKS[] = {lmask<true>(2), lmask<true>(3),
                             lmask<true>(4), lmask<true>(5)};
constexpr grid_t HMASKS[] = {lmask<false>(2), lmask<false>(3),
                             lmask<false>(4), lmask<false>(5)};

// shift mask such that 0,0 is moved to r,c
inline constexpr grid_t shift2d(grid_t mask, int x, int y) {
    return mask << static_cast<grid_t>(y * BOARD_WIDTH + x);
}

inline constexpr grid_t mk_ship_mask(int size, bool vert, int x, int y) {
    return shift2d((vert ? VMASKS : HMASKS)[size - 2], x, y);
}

// REQ_MASK[i][p][j][vert] will represent the possible positions for ship j given that 
// there exists a ship i at square p. 
// REQ_MASK[i][0][i][vert] = initial starting point for ship type i given boundaries.
// this has huge wastage as ships 2 and 3 are identical.
grid_t REQ_MASKS[NUM_SHIPS][BOARD_SIZE*2][NUM_SHIPS][2] = {{{{0}}}};
constexpr void init_req_masks() {
    for (int base_ship = 0; base_ship < NUM_SHIPS; base_ship++) {
        int base_size = SHIP_SIZES[base_ship];
        for (int place = 0; place < BOARD_SIZE * 2; place++) {
            int base_sq = place % BOARD_SIZE;
            bool base_vert = place / BOARD_SIZE;
            int base_x = base_sq % BOARD_WIDTH;
            int base_y = base_sq / BOARD_WIDTH;

            if ((!base_vert && base_x + base_size > BOARD_WIDTH)
                || (base_vert && base_y + base_size > BOARD_HEIGHT))
                continue; // base doesnt even fit.

            grid_t base = mk_ship_mask(base_size, base_vert, base_x, base_y);
            for (int query = 0; query < NUM_SHIPS * 2; query++) {
                int place_ship = query % NUM_SHIPS;
                int place_size = SHIP_SIZES[place_ship];
                bool place_vert = query / NUM_SHIPS;

                // if we are querying about placing a ship already placed
                // we reinterpret to be about placing the ship onto an empty board.
                grid_t eff_base = (place_ship == base_ship ? 0 : base);

                REQ_MASKS[base_ship][place][place_ship][place_vert?1:0] = 0;
                for (int qsq = 0; qsq < BOARD_SIZE; qsq++) {
                    int p_x = qsq % BOARD_WIDTH;
                    int p_y = qsq / BOARD_WIDTH;
                    if ((!place_vert && p_x+ place_size > BOARD_WIDTH)
                        || (place_vert && p_y + place_size > BOARD_HEIGHT)
                        || (eff_base & mk_ship_mask(place_size, place_vert, p_x, p_y)))
                        continue;
                    
                    REQ_MASKS[base_ship][place][place_ship][place_vert?1:0]
                        |= mk_mask(p_x, p_y);
                }
            }
        }
    }
}

inline constexpr int popcnt(grid_t g) {
    uint64_t lo = g;
    uint64_t hi = (g >> static_cast<grid_t>(64));
    return std::popcount(lo) + std::popcount(hi);
}

inline constexpr int countr_zero(grid_t g) {
    uint64_t lo = g;
    uint64_t hi = (g >> static_cast<grid_t>(64));
    return lo ? std::countr_zero(lo) : 64 + std::countr_zero(hi);
}

inline constexpr grid_t isolate_nth_bit(grid_t g, int bit) {
    for (int i = 0; i < bit; i++)
        g &= (g - 1);
    return g & ~(g - 1);
}

struct BSConfig2 {
    uint8_t pad[8 - NUM_SHIPS];
    uint8_t ships[NUM_SHIPS];

    // to_bytes() and to_bytes_constexpr() should be equivalent
    inline uint64_t to_bytes() {
        return *reinterpret_cast<uint64_t *>(this);
    }

    inline constexpr uint64_t to_bytes_constexpr() {
        uint64_t ret = 0;
        for (int i = 0; i < NUM_SHIPS; i++) {
            ret |= ships[i];
            ret <<= 8;
        }

        return ret;
    }

    inline bool ship_is_vert(int s) {
        return ships[s] / BOARD_SIZE;
    }

    inline void set_ship_vert(uint8_t s, bool vert) {
        ships[s] %= BOARD_SIZE;
        if (vert)
            ships[s] += BOARD_SIZE;
    }
};

#endif
