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

constexpr int SHIP_SIZES[] = {5,4,3,3,2};
constexpr int NUM_SHIPS = 5;
constexpr int NUM_SIZES = 4;
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

inline void dump_board(grid_t grid, bool full = false) {
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

inline constexpr grid_t mk_ship_mask(int size, bool vert, int sq) {
    return (vert ? VMASKS : HMASKS)[size - 2] << static_cast<grid_t>(sq);
}

// REQ_MASK[i][p][j][vert] will represent the possible positions for ship with size j with vert given that
// there exists a ship of size i at square/vertical-state p.
extern grid_t REQ_MASKS[NUM_SIZES][BOARD_SIZE*2][NUM_SIZES][2];

// REQ_HIT_MASKS[i][vert][occ] will represent the possible positions for a ship of size i with vert
// to satisfy the condition that the square at occ is occupied.
// Index [i][vert][BOARD_SIZE] represents empty board/no condition at all.
extern grid_t REQ_HIT_MASKS[NUM_SIZES][2][BOARD_SIZE+1]; // idx at BOARD_SIZE Is empty board
constexpr void init_req_masks() {
    for (int base_size = 2; base_size < 6; base_size++) {
        for (int place = 0; place < BOARD_SIZE * 2; place++) {
            int base_sq = place % BOARD_SIZE;
            bool base_vert = place / BOARD_SIZE;
            int base_x = base_sq % BOARD_WIDTH;
            int base_y = base_sq / BOARD_WIDTH;

            if ((!base_vert && base_x + base_size > BOARD_WIDTH)
                || (base_vert && base_y + base_size > BOARD_HEIGHT))
                continue; // base doesnt even fit.

            grid_t base = mk_ship_mask(base_size, base_vert, base_x, base_y);
            for (int query = 0; query < (NUM_SHIPS-1) * 2; query++) {
                int place_size = 2 + (query % (NUM_SHIPS - 1));
                bool place_vert = query / (NUM_SHIPS - 1);

                REQ_MASKS[base_size-2][place][place_size-2][place_vert?1:0] = 0;
                for (int qsq = 0; qsq < BOARD_SIZE; qsq++) {
                    int p_x = qsq % BOARD_WIDTH;
                    int p_y = qsq / BOARD_WIDTH;
                    if ((!place_vert && p_x+ place_size > BOARD_WIDTH)
                        || (place_vert && p_y + place_size > BOARD_HEIGHT)
                        || (base & mk_ship_mask(place_size, place_vert, p_x, p_y)))
                        continue;
                    
                    REQ_MASKS[base_size-2][place][place_size-2][place_vert?1:0]
                        |= mk_mask(p_x, p_y);
                }
            }
        }
    }

    for (int ship_size = 2; ship_size < 6; ship_size++) {
        for (int occ_sq = 0; occ_sq < BOARD_SIZE+1; occ_sq++) {
            grid_t occ = occ_sq < BOARD_SIZE ? mk_mask(occ_sq) : ~static_cast<grid_t>(0);

            REQ_HIT_MASKS[ship_size - 2][0][occ_sq] = 0;
            REQ_HIT_MASKS[ship_size - 2][1][occ_sq] = 0;

            for (int plsq = 0; plsq < BOARD_SIZE*2; plsq++) {
                bool vert = plsq / BOARD_SIZE;
                int p_x = (plsq % BOARD_SIZE) % BOARD_WIDTH;
                int p_y = (plsq % BOARD_SIZE) / BOARD_WIDTH;
                grid_t try_place = mk_ship_mask(ship_size, vert, p_x, p_y);
                if ((!vert && p_x+ ship_size > BOARD_WIDTH)
                    || (vert && p_y + ship_size > BOARD_HEIGHT)
                    || !(occ & try_place))
                    continue;
                REQ_HIT_MASKS[ship_size - 2][vert?1:0][occ_sq] |= mk_mask(p_x, p_y);
            }
        }
    }
}

inline constexpr int popcnt(grid_t g) {
//    return std::popcount(g);
    uint64_t lo = g;
    uint64_t hi = (g >> static_cast<grid_t>(64));
    return std::popcount(lo) + std::popcount(hi);
}

inline constexpr int countr_zero(grid_t g) {
//    return std::countr_zero(g);
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

struct BSSampler {
    grid_t hits = 0;
    grid_t misses = 0;

    // cached masks for requiring to satisfy misses.
    // this cannot be used for hits as we want to dynamically calculate
    // as we satisfy misses and know what is left.
    grid_t req_miss_masks[NUM_SHIPS-1][2] = {{~static_cast<grid_t>(0)}};

    std::atomic_uint_fast64_t config_counts[BOARD_SIZE*2][NUM_SHIPS-1] = {{0}};
    std::atomic_uint_fast64_t total = 0;
    std::atomic_uint_fast64_t its = 0;

    int hit_anchor_sq = -1; // only for first ship placed.

    std::mutex impossible_mtx{};
    std::unordered_set<uint64_t> impossible{};

    double probs[BOARD_SIZE] = {0};
    double max_prob = 1;
    double sumprobs = 0;
    int next_guess_sq = -1;

    inline void clear() {
        impossible.clear();
        total = 0;
        its = 0;
        for (double & prob : probs)
            prob = 0;
        for (auto &config_count : config_counts)
            for (auto &y : config_count)
                y = 0;
    }

    void enumerate();
    void multithread_enum();

    void create_miss_masks(std::mt19937_64 &rng, bool use_config_counts);

    void try_random(std::mt19937_64 &rng);

    void config_to_probs();

    void multithread_randsample(uint32_t max);
};


#endif
