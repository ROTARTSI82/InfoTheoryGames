
#include <fstream>
#include "battleship.hpp"

void test_enum() {
    std::cout << "atomic int = " << sizeof(std::atomic_uint32_t) << '\n';
    std::random_device dev;
    std::mt19937_64 rng(dev());
    std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,BOARD_SIZE-1); // distribution in range [1, 6]

    std::cout << sizeof(grid_t);
    std::cout << "\tHi\n";
    BSSampler e{};
    e.hits = e.misses = 0;
    // for (int i = 0; i < 5; i++)
    //     e.hits |= mk_mask(dist(rng));
    // for (int i = 0; i < 5; i++)
    //     e.misses |= mk_mask(dist(rng));
    // e.random_populate_hit_anchors(rng);
    // e.launch_multithread();
    e.enumerate();

    std::cout << "===== [ HITS ] =====\n";
    dump_board(e.hits);
    std::cout << "======= [ MISS ] =======\n";
    dump_board(e.misses);

    std::cout << "count = " << e.total << '\n';

    for (int y = 0; y < BOARD_HEIGHT; y++) {
        for (int x = 0; x < BOARD_WIDTH; x++) {
            float prob = e.probs[y * BOARD_WIDTH + x] / (float) e.total;
            std::cout << '\t' << prob;
        }
        std::cout << '\n';
    }
}

void test_randsample() {
    std::random_device dev;
    std::mt19937_64 rng(dev());
    std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,BOARD_SIZE-1); // distribution in range [1, 6]

    BSSampler e;
    e.clear();
    e.hits = e.misses = 1;
    while (e.hits & e.misses) {
        e.hits = e.misses = 0;
        for (int i = 0; i < 5; i++)
            e.hits |= mk_mask(dist(rng));
        for (int i = 0; i < 5; i++)
            e.misses |= mk_mask(dist(rng));
    }

    e.create_miss_masks(rng, false);

    std::cout << "===== [ HITS ] =====\n";
    dump_board(e.hits, true);
    std::cout << "======= [ MISS ] =======\n";
    dump_board(e.misses, true);

    // return;

    // e.its = 0;
    // while (++e.its && e.total < 4096)
    //     std::cout << e.try_random(rng) << std::flush;

    e.multithread_randsample(4096 * 128);

    std::cout << '\n';

    std::cout << "===== [ HITS ] =====\n";
    dump_board(e.hits);
    std::cout << "======= [ MISS ] =======\n";
    dump_board(e.misses);

    for (int y = 0; y < BOARD_WIDTH; y++) {
        for (int x = 0; x < BOARD_WIDTH; x++) {
            std::cout << '\t' << std::setprecision(3) << static_cast<double>(e.probs[y * BOARD_WIDTH + x]) / e.total;
        }
        std::cout << '\n';
    }

    std::cout << "sample size = " << e.total << " in iterations = " << e.its << '\n';
    std::cout << "num impossible = " << e.impossible.size() << '\n';
}

int run_battleship();
grid_t REQ_MASKS[NUM_SHIPS-1][BOARD_SIZE*2][NUM_SHIPS-1][2] = {{{{0}}}};
grid_t REQ_HIT_MASKS[NUM_SHIPS-1][2][BOARD_SIZE+1] = {{{0}}};
int main() {
    init_req_masks();

    // for (int i = 0; i < NUM_SHIPS; i++)
    //     for (int j = 0; j < BOARD_SIZE * 2; j++)
    //         for (int k = 0; k < NUM_SHIPS * 2; k++) {
    //             std::cout << "\ngiven " << i << " at " << j << " valid cand for " << k << '\n';
    //             dump_board(REQ_MASKS[i][j][k%NUM_SHIPS][k/NUM_SHIPS], true);
    //         }

//    for (int i = 0; i < NUM_SHIPS - 1; i++)
//        for (int j = 0; j < BOARD_SIZE+1; j++)
//            for (int k = 0; k < 2; k++) {
//                std::cout << "\nship size = " << i + 2 << ", occ = " << j << ", vert = " << k << '\n';
//                dump_board(REQ_HIT_MASKS[i][k][j], true);
//            }

    // return 0;
    std::cout << "config size = " << sizeof(BSConfig2) << '\n';
    std::cout << "sampler size = " << sizeof(BSSampler) << '\n';
    // test_randsample();
    run_battleship();
//    test_enum();
}
