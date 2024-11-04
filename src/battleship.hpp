#pragma once

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

inline constexpr grid_t mk_mask(int r, int c) {
    return mk_mask(r * BOARD_WIDTH + c);
}

template <bool VERT>
inline constexpr grid_t lmask(int n) {
    grid_t ret = 0;
    for (int i = 0; i < n; i++)
        ret |= VERT ? mk_mask(i,0) : mk_mask(i);
    return ret;
}

void dump_board(grid_t grid, bool full = false) {
    for (int y = 0; y < (full ? 13 : BOARD_HEIGHT); y++) {
        std::cout << "\n" << y << "\t";
        for (int x = 0; x < BOARD_WIDTH; x++)
            std::cout << ((grid & mk_mask(y, x)) ? 'x' : '.');
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

inline constexpr int popcnt(grid_t g) {
    int ret = 0;
    while (g) {
        ret += 1;
        g &= (g - 1);
    }
    return ret;
}

inline constexpr grid_t isolate_nth_bit(grid_t g, int bit) {
    for (int i = 0; i < bit; i++)
        g &= (g - 1);
    return g & ~(g - 1);
}


struct BSConfig {
    struct ship_t {
        uint8_t r : 4;
        uint8_t c : 4;
    };

    ship_t ships[NUM_SHIPS]; // 5 bytes
    uint8_t vert_state = 0; // bitmap of which ships are vertical
    uint8_t pad[2] = {0};

    uint64_t to_bytes() {
        return *reinterpret_cast<uint64_t *>(this);
    }

    inline bool ship_is_vert(int s) {
        return (vert_state >> s) & 1;
    }

    inline void set_ship_vert(uint8_t s, bool vert) {
        if (vert)
            vert_state |= static_cast<uint8_t>(1) << s;
        else
            vert_state &= ~(static_cast<uint8_t>(1) << s);
    }
};
