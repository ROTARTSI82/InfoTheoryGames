// Dear ImGui: standalone example application for SDL2 + SDL_Renderer
// (SDL is a cross-platform general purpose library for handling windows, inputs, OpenGL/Vulkan/Metal graphics context creation, etc.)

// Learn about Dear ImGui:
// - FAQ                  https://dearimgui.com/faq
// - Getting Started      https://dearimgui.com/getting-started
// - Documentation        https://dearimgui.com/docs (same as your local docs/ folder).
// - Introduction, links and more at the top of imgui.cpp

// Important to understand: SDL_Renderer is an _optional_ component of SDL2.
// For a multi-platform app consider using e.g. SDL+DirectX on Windows and SDL+OpenGL on Linux/OSX.

#pragma once

#ifndef INCLUDE_GUI_CPP
#define INCLUDE_GUI_CPP

#include "imgui.h"
#include "imgui_impl_sdl2.h"
#include "imgui_impl_sdlrenderer2.h"
#include <stdio.h>
#include <SDL.h>
#include <cstdlib>

#include "battleship.hpp"


// Main code
int run_battleship()
{
    // Setup SDL
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER) != 0)
    {
        printf("Error: %s\n", SDL_GetError());
        return -1;
    }

    // From 2.0.18: Enable native IME.
#ifdef SDL_HINT_IME_SHOW_UI
    SDL_SetHint(SDL_HINT_IME_SHOW_UI, "1");
#endif

    // Create window with SDL_Renderer graphics context
    SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
    SDL_Window* window = SDL_CreateWindow("Dear ImGui SDL2+SDL_Renderer example", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1280, 720, window_flags);
    if (window == nullptr)
    {
        printf("Error: SDL_CreateWindow(): %s\n", SDL_GetError());
        return -1;
    }
    SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
    if (renderer == nullptr)
    {
        SDL_Log("Error creating SDL_Renderer!");
        return -1;
    }
    //SDL_RendererInfo info;
    //SDL_GetRendererInfo(renderer, &info);
    //SDL_Log("Current SDL_Renderer: %s", info.name);

    // Setup Dear ImGui context
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();
    //ImGui::StyleColorsLight();

    // Setup Platform/Renderer backends
    ImGui_ImplSDL2_InitForSDLRenderer(window, renderer);
    ImGui_ImplSDLRenderer2_Init(renderer);

    // Load Fonts
    // - If no fonts are loaded, dear imgui will use the default font. You can also load multiple fonts and use ImGui::PushFont()/PopFont() to select them.
    // - AddFontFromFileTTF() will return the ImFont* so you can store it if you need to select the font among multiple.
    // - If the file cannot be loaded, the function will return a nullptr. Please handle those errors in your application (e.g. use an assertion, or display an error and quit).
    // - The fonts will be rasterized at a given size (w/ oversampling) and stored into a texture when calling ImFontAtlas::Build()/GetTexDataAsXXXX(), which ImGui_ImplXXXX_NewFrame below will call.
    // - Use '#define IMGUI_ENABLE_FREETYPE' in your imconfig file to use Freetype for higher quality font rendering.
    // - Read 'docs/FONTS.md' for more instructions and details.
    // - Remember that in C/C++ if you want to include a backslash \ in a string literal you need to write a double backslash \\ !
    //io.Fonts->AddFontDefault();
    //io.Fonts->AddFontFromFileTTF("c:\\Windows\\Fonts\\segoeui.ttf", 18.0f);
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/DroidSans.ttf", 16.0f);
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/Roboto-Medium.ttf", 16.0f);
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/Cousine-Regular.ttf", 15.0f);
    //ImFont* font = io.Fonts->AddFontFromFileTTF("c:\\Windows\\Fonts\\ArialUni.ttf", 18.0f, nullptr, io.Fonts->GetGlyphRangesJapanese());
    //IM_ASSERT(font != nullptr);

    // Our state
    bool show_demo_window = true;
    bool show_another_window = false;
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

    // Main loop
    bool done = false;

    grid_t occ_ships[NUM_SHIPS] = {0};
    BSSampler sampler{};
    int maxIt = 0;
    float maxprob = 1.0;
    int maxprob_sq = 0;
    std::random_device dev;
    std::mt19937_64 rng(dev());
    while (!done)
    {
        // Poll and handle events (inputs, window resize, etc.)
        // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
        // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application, or clear/overwrite your copy of the mouse data.
        // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application, or clear/overwrite your copy of the keyboard data.
        // Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            ImGui_ImplSDL2_ProcessEvent(&event);
            if (event.type == SDL_QUIT)
                done = true;
            if (event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_CLOSE && event.window.windowID == SDL_GetWindowID(window))
                done = true;
        }
        if (SDL_GetWindowFlags(window) & SDL_WINDOW_MINIMIZED)
        {
            SDL_Delay(10);
            continue;
        }

        // Start the Dear ImGui frame
        ImGui_ImplSDLRenderer2_NewFrame();
        ImGui_ImplSDL2_NewFrame();
        ImGui::NewFrame();

        // 1. Show the big demo window (Most of the sample code is in ImGui::ShowDemoWindow()! You can browse its code to learn more about Dear ImGui!).
        if (show_demo_window)
            ImGui::ShowDemoWindow(&show_demo_window);

        // 2. Show a simple window that we create ourselves. We use a Begin/End pair to create a named window.
        {
            static float f = 0.0f;
            static int counter = 0;

            ImGui::Begin("Battleship!");
            static int to_place = 0;
            static bool placemode = true;
            static bool place_vert = false;

            ImGui::Text("Ship to place (by size)");
            constexpr std::array<std::string, NUM_SHIPS> names = {"Carrier ", "Battleship ", "Destroyer ", "Submarine ", "Patrol Boat "}; 
            constexpr std::array<ImColor, NUM_SHIPS> ship_colors = {ImColor(255, 255, 0), ImColor(0,255,0), ImColor(0,0,255), ImColor(0,255,255),ImColor(255,0,255)};
            ImColor empty(128, 128, 128);
            for (int i = 0; i < NUM_SHIPS; i++) {
                ImGui::PushID(i);
                std::string s = " (" + std::to_string(SHIP_SIZES[i]) + ")";
                ImGui::RadioButton(names[i].c_str(), &to_place, i);
                ImGui::SameLine();
                ImGui::TextColored(ship_colors[i].Value, "%s", s.c_str());
                if (i < NUM_SHIPS - 1) ImGui::SameLine();
                ImGui::PopID();
            }

            ImGui::Separator();

            ImGui::Checkbox("Ship Placing Mode", &placemode);
            ImGui::Checkbox("Place Vertically", &place_vert);

            ImGui::SliderInt("Num Iterations", &maxIt, 1, 4096*128);

            if (ImGui::Button("Next Move")) {
                grid_t totocc = 0;
                for (int i = 0; i < NUM_SHIPS; i++)
                    totocc |= occ_ships[i];
                if (mk_mask(maxprob_sq) & totocc)
                    sampler.hits |= mk_mask(maxprob_sq);
                else
                    sampler.misses |= mk_mask(maxprob_sq);
                std::cout << "nextMove\n";
            }
            ImGui::SameLine();
            if (ImGui::Button("Calculate Random Sample")) {
                sampler.clear();
                sampler.create_miss_masks();
                sampler.multithread_randsample(maxIt);
                maxprob = 0;
                for (int s = 0; s < BOARD_SIZE; s++) {
                    float p = sampler.counts[s] / (float) sampler.total;
                    if (p > maxprob && p < 1) {
                        maxprob_sq = s;
                        maxprob = p;
                    }
                }
                std::cout << "randsample\n";
            }
            if (ImGui::Button("Enumerate")) {
                sampler.clear();
                sampler.create_miss_masks();
                sampler.multithread_enum();
                maxprob = 0;
                for (int s = 0; s < BOARD_SIZE; s++) {
                    float p = sampler.counts[s] / (float) sampler.total;
                    if (p > maxprob && p < 1) {
                        maxprob_sq = s;
                        maxprob = p;
                    }
                }
                std::cout << "randsample\n";
            }

            ImGui::Text("Found/Iterations: %i/%i", sampler.total.load(), sampler.its.load());

            ImGuiTableFlags flags = ImGuiTableFlags_Borders | ImGuiTableFlags_SizingFixedSame;
            if (ImGui::BeginTable("Battleship", BOARD_WIDTH, flags)) {
                for (int sq = 0; sq < BOARD_SIZE; sq++) {
                    ImGui::PushID(sq);
                    ImGui::TableNextColumn();
                    float prob = sampler.counts[sq] / (float) sampler.total.load();
                    ImVec4 col = ImVec4(prob/maxprob, 1.0 - prob/maxprob, 0.0, 1.0);
                    ImGui::TextColored(col, "Prob: %f", prob);

                    for (int shp = 0; shp < NUM_SHIPS; shp++) {
                        if (mk_mask(sq) & occ_ships[shp]) {
                            ImGui::TextColored(ship_colors[shp].Value, "%s", names[shp].c_str());
                            break;
                        }
                    }
                    if (mk_mask(sq) & sampler.hits)
                            ImGui::TextColored(ImVec4(1,0,0,1), "HIT");
                    if (mk_mask(sq) & sampler.misses)
                        ImGui::TextColored(empty.Value, "MISS");

                    if (placemode && ImGui::Button("place")) {
                        std::cout << "place " << sq << '\n';
                        occ_ships[to_place] = mk_ship_mask(SHIP_SIZES[to_place], place_vert, sq % BOARD_WIDTH, sq / BOARD_WIDTH);
                    }
                    ImGui::PopID();
                }
                ImGui::EndTable();
            }

            ImGui::End();
        }

        // 3. Show another simple window.
        if (show_another_window)
        {
            ImGui::Begin("Another Window", &show_another_window);   // Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
            ImGui::Text("Hello from another window!");
            if (ImGui::Button("Close Me"))
                show_another_window = false;
            ImGui::End();
        }

        // Rendering
        ImGui::Render();
        SDL_RenderSetScale(renderer, io.DisplayFramebufferScale.x, io.DisplayFramebufferScale.y);
        SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
        SDL_RenderClear(renderer);
        ImGui_ImplSDLRenderer2_RenderDrawData(ImGui::GetDrawData(), renderer);
        SDL_RenderPresent(renderer);
    }

    // Cleanup
    ImGui_ImplSDLRenderer2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}

#endif