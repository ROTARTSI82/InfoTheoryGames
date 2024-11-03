cmake_minimum_required(VERSION 3.29)
project(InfoGames)

add_executable(InfoGames src/main.cpp)
target_include_directories(InfoGames PUBLIC src)
target_compile_options(InfoGames PUBLIC -std=c++20 -mtune=native -march=native -Ofast -fno-exceptions -s)

# -g3 -glldb -fno-omit-frame-pointer -fsanitize=address 

target_link_options(InfoGames PUBLIC -std=c++20 -mtune=native -march=native -Ofast -fno-exceptions -s)
