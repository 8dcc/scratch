
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <SDL2/SDL.h>

#define WINDOW_W 640
#define WINDOW_H 480
#define FPS      60

#define COLOR_GRID 0x111111
#define COLOR_AXIS 0x555555

/* How many pixels on the screen represent each unit in our graph */
#define GRAPH_STEP 20

typedef double (*PlotFuncPtr)(double x);

/*----------------------------------------------------------------------------*/
/* Globals */

static SDL_Window* g_window     = NULL;
static SDL_Renderer* g_renderer = NULL;

static bool g_draw_grid = true;

/*----------------------------------------------------------------------------*/
/* The actual plot functions. Takes X and returns Y. */

#define FUNC_NUM 3

static double plotted0(double x) {
    return sin(x) * 2;
}

static double plotted1(double x) {
    return cos(x) * 2;
}

static double plotted2(double x) {
    return pow(x, 2);
}

/*----------------------------------------------------------------------------*/
/* Misc helper functions */

static void die(const char* fmt, ...) {
    va_list va;
    va_start(va, fmt);

    vfprintf(stderr, fmt, va);
    putc('\n', stderr);

    if (g_window != NULL)
        SDL_DestroyWindow(g_window);

    SDL_Quit();
    exit(1);
}

/*----------------------------------------------------------------------------*/
/* SDL helper functions */

static inline void set_render_color(SDL_Renderer* rend, uint32_t col) {
    const uint8_t r = (col >> 16) & 0xFF;
    const uint8_t g = (col >> 8) & 0xFF;
    const uint8_t b = (col >> 0) & 0xFF;
    const uint8_t a = 255;
    SDL_SetRenderDrawColor(rend, r, g, b, a);
}

static void draw_grid(void) {
    if (!g_draw_grid)
        return;

    int win_w, win_h;
    SDL_GetWindowSize(g_window, &win_w, &win_h);

    const int center_x = win_w / 2;
    const int center_y = win_h / 2;

    for (int y = center_y; y < win_h; y += GRAPH_STEP)
        SDL_RenderDrawLine(g_renderer, 0, y, win_w, y);

    for (int y = center_y; y > 0; y -= GRAPH_STEP)
        SDL_RenderDrawLine(g_renderer, 0, y, win_w, y);

    for (int x = center_x; x < win_w; x += GRAPH_STEP)
        SDL_RenderDrawLine(g_renderer, x, 0, x, win_h);

    for (int x = center_x; x > 0; x -= GRAPH_STEP)
        SDL_RenderDrawLine(g_renderer, x, 0, x, win_h);
}

static void draw_axis(void) {
    if (!g_draw_grid)
        return;

    int win_w, win_h;
    SDL_GetWindowSize(g_window, &win_w, &win_h);

    const int center_x = win_w / 2;
    const int center_y = win_h / 2;

    SDL_RenderDrawLine(g_renderer, center_x, 0, center_x, win_h);
    SDL_RenderDrawLine(g_renderer, 0, center_y, win_w, center_y);
}

static void plot_func(PlotFuncPtr funcs[FUNC_NUM], uint32_t colors[FUNC_NUM]) {
    int win_w, win_h;
    SDL_GetWindowSize(g_window, &win_w, &win_h);

    const int half_w = win_w / 2;
    const int half_h = win_h / 2;

    for (int i = 0; i < FUNC_NUM; i++) {
        set_render_color(g_renderer, colors[i]);

        /* Iterate each screen pixel in the horizontal */
        int prev_y = 0;
        for (int x = -half_w; x < half_w; x++) {
            /* The X coordinate in our virtual space. Will be used when calling
             * the plot function. */
            const double virtual_x = (double)x / GRAPH_STEP;

            /* The real X and Y coordinates in our screen */
            const int render_x = half_w + x;
            const int render_y = half_h - funcs[i](virtual_x) * GRAPH_STEP;

            /* Draw the line from the previous point to the current one */
            SDL_RenderDrawLine(g_renderer, render_x - 1, prev_y, render_x,
                               render_y);

            /* Save the Y coordinate for the next iteration */
            prev_y = render_y;
        }
    }
}

/*----------------------------------------------------------------------------*/
/* Main function */

int main(void) {
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0)
        die("Unable to start SDL.");

    /* Create SDL window */
    g_window = SDL_CreateWindow("Potting function", SDL_WINDOWPOS_CENTERED,
                                SDL_WINDOWPOS_CENTERED, WINDOW_W, WINDOW_H,
                                SDL_WINDOW_RESIZABLE);
    if (!g_window)
        die("Error creating SDL window.");

    /* Create SDL renderer */
    g_renderer =
      SDL_CreateRenderer(g_window, -1,
                         SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!g_renderer) {
        SDL_DestroyWindow(g_window);
        die("Error creating SDL renderer.");
    }

    /* Main loop */
    bool running = true;
    while (running) {
        /* Parse SDL events */
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
                case SDL_QUIT: {
                    running = false;
                } break;

                case SDL_KEYDOWN: {
                    switch (event.key.keysym.scancode) {
                        case SDL_SCANCODE_ESCAPE:
                        case SDL_SCANCODE_Q: {
                            running = false;
                        } break;

                        case SDL_SCANCODE_G: {
                            g_draw_grid = !g_draw_grid;
                        } break;

                        case SDL_SCANCODE_F11:
                        case SDL_SCANCODE_F: {
                            /* Toggle fullscreen */
                            uint32_t new_flags =
                              (SDL_GetWindowFlags(g_window) &
                               SDL_WINDOW_FULLSCREEN_DESKTOP)
                                ? 0
                                : SDL_WINDOW_FULLSCREEN_DESKTOP;

                            SDL_SetWindowFullscreen(g_window, new_flags);
                        } break;

                        default:
                            break;
                    }
                } break;

                default:
                    break;
            }
        }

        /* Clear window */
        set_render_color(g_renderer, 0x000000);
        SDL_RenderClear(g_renderer);

        /* Draw background grid */
        set_render_color(g_renderer, COLOR_GRID);
        draw_grid();

        /* Plot the actual functions */
        PlotFuncPtr funcs[FUNC_NUM] = { plotted0, plotted1, plotted2 };
        uint32_t colors[FUNC_NUM]   = { 0xFF0000, 0x0000FF, 0x00FF00 };
        plot_func(funcs, colors);

        /* Draw the axis on top of the graph */
        set_render_color(g_renderer, COLOR_AXIS);
        draw_axis();

        /* Send to renderer and delay depending on FPS */
        SDL_RenderPresent(g_renderer);
        SDL_Delay(1000 / FPS);
    }

    SDL_DestroyRenderer(g_renderer);
    SDL_DestroyWindow(g_window);
    SDL_Quit();

    return 0;
}
