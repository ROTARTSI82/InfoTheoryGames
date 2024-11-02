#include <iostream>
#include "battleship.cpp"
#include <fstream>

int main() {
    std::cout << sizeof(grid_t);
    std::cout << "\tHi\n";
    StateEnumerator e{};
    std::cout << "lf max_buckets=" << e.gamestates.max_bucket_count() 
        << ", load=" << e.gamestates.max_load_factor() 
        << ", size=" << e.gamestates.max_size() 
        << ", buckets=" << e.gamestates.bucket_count() << '\n';
    // e.launch_multithread();
    e.enumerate(0, 0, 4);
    std::cout << "states: " << e.gamestates.size() << '\n';
    std::cout << "lf buckets=" << e.gamestates.max_bucket_count() 
              << ", load=" << e.gamestates.max_load_factor() 
              << ", size=" << e.gamestates.max_size() 
              << ", buckets=" << e.gamestates.bucket_count() << '\n';

    auto size = e.gamestates.size();
    grid_t *convert = new grid_t[size];
    std::cout << "beginning conversion to vector\n";
    grid_t *cur = convert;
    for (auto x : e.gamestates)
        *cur++ = x;
    std::cout << "vectorized. writing to file\n";

    std::ofstream stream("out.bin", std::ios::binary);
    stream.write(reinterpret_cast<char *>(convert), sizeof(grid_t) * size);
    std::cout << "done\n";
    delete[] convert;
}