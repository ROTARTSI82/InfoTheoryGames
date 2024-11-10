cmake_minimum_required(VERSION 3.29)
project(InfoGames)

file(GLOB IMGUI_IMPL submodules/imgui/*.cpp)
add_executable(InfoGames src/main.cpp 
                ${IMGUI_IMPL} submodules/imgui/backends/imgui_impl_sdl2.cpp submodules/imgui/backends/imgui_impl_sdlrenderer2.cpp)
target_include_directories(InfoGames PUBLIC src submodules/imgui submodules/imgui/backends)
target_compile_options(InfoGames PUBLIC -std=c++20 -mtune=native -march=native -Ofast -fno-exceptions -Wall -Wextra -Wpedantic) # -g3 -glldb -fno-omit-frame-pointer -fsanitize=address 

find_package(SDL2 REQUIRED)

# -g3 -glldb -fno-omit-frame-pointer -fsanitize=address 

target_link_options(InfoGames PUBLIC -std=c++20 -mtune=native -march=native -Ofast -fno-exceptions -Wall -Wextra -Wpedantic) # -g3 -glldb -fno-omit-frame-pointer -fsanitize=address
target_link_libraries(InfoGames PUBLIC SDL2::SDL2)
target_include_directories(InfoGames PUBLIC SDL2::SDL2 ${SDL_INCLUDE_DIRS})
