
#include "battleship_enum.cpp"
#include "battleship_randsample.cpp"
#include <fstream>


void test_enum() {
    std::cout << "atomic int = " << sizeof(std::atomic_uint32_t) << '\n';
    std::random_device dev;
    std::mt19937_64 rng(dev());
    std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,BOARD_SIZE-1); // distribution in range [1, 6]

    std::cout << sizeof(grid_t);
    std::cout << "\tHi\n";
    StateEnumerator e{};
    for (int i = 0; i < 5; i++)
        e.hits |= mk_mask(dist(rng));
    for (int i = 0; i < 5; i++)
        e.misses |= mk_mask(dist(rng));
    e.random_populate_hit_anchors(rng);
    std::cout << "lf max_buckets=" << e.gamestates.max_bucket_count() 
        << ", load=" << e.gamestates.max_load_factor() 
        << ", size=" << e.gamestates.max_size() 
        << ", buckets=" << e.gamestates.bucket_count() << '\n';
    e.launch_multithread();
    // e.enumerate(0, 0, 4);

    // grid_t *convert = new grid_t[size];
    // std::cout << "beginning conversion to vector\n";
    // grid_t *cur = convert;
    for (auto x : e.gamestates) {
        // *cur++ = x;
        dump_board(x);
    }
    // std::cout << "vectorized. writing to file\n";

    // std::ofstream stream("out.bin", std::ios::binary);
    // stream.write(reinterpret_cast<char *>(convert), sizeof(grid_t) * size);
    // std::cout << "done\n";
    // delete[] convert;

    std::cout << "states: " << e.gamestates.size() << '\n';
    std::cout << "lf buckets=" << e.gamestates.max_bucket_count() 
              << ", load=" << e.gamestates.max_load_factor() 
              << ", size=" << e.gamestates.max_size() 
              << ", buckets=" << e.gamestates.bucket_count() << '\n';

    std::cout << " ==== [anchor] =====\n";
    dump_board(e.hit_anchors[0]);
    std::cout << "===== [ HITS ] =====\n";
    dump_board(e.hits);
    std::cout << "======= [ MISS ] =======\n";
    dump_board(e.misses);
}

void test_randsample() {
    std::random_device dev;
    std::mt19937_64 rng(dev());
    std::uniform_int_distribution<std::mt19937_64::result_type> dist(0,BOARD_SIZE-1); // distribution in range [1, 6]

    BSRandSample e;
    e.clear();
    for (int i = 0; i < 5; i++)
        e.hits |= mk_mask(dist(rng));
    for (int i = 0; i < 5; i++)
        e.misses |= mk_mask(dist(rng));

    for (int i = 0; i < 4096; i++)
        std::cout << e.try_random(rng);
    std::cout << '\n';

    std::cout << "===== [ HITS ] =====\n";
    dump_board(e.hits);
    std::cout << "======= [ MISS ] =======\n";
    dump_board(e.misses);

    for (int y = 0; y < BOARD_WIDTH; y++) {
        for (int x = 0; x < BOARD_WIDTH; x++) {
            std::cout << '\t' << std::setprecision(0.01) << static_cast<double>(e.counts[y * BOARD_WIDTH + x]) / e.total;
        }
        std::cout << '\n';
    }

    std::cout << "sample size = " << e.total << '\n';
    std::cout << "num impossible = " << e.impossible.size() << '\n';
}

int main() {
    test_randsample();
}