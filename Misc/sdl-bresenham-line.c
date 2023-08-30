/* For commented version, see:
 *   https://github.com/8dcc/header-img-viewer/blob/main/src/main.c
 *   https://github.com/8dcc/c-stuff/blob/main/Misc/bresenham-line.c
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SDL.h"

#define DRAW_AXIS
#define GRAPH_MARGIN 1
#define FPS          30

#define ABS(n)     ((n) < 0 ? (-(n)) : (n))
#define MAX(a, b)  ((a) < (b) ? b : a)
#define MIN(a, b)  ((a) > (b) ? b : a)
#define DEG2RAD(n) ((n)*M_PI / 180.0f)
#define RAD2DEG(n) ((n)*180.0f / M_PI)

static void bresenham_line(bool* arr, int arr_h, int arr_w, int x0, int y0,
                           int x1, int y1);
static void bresenham_x(bool* arr, int arr_h, int arr_w, int x0, int y0, int x1,
                        int y1);
static void bresenham_y(bool* arr, int arr_h, int arr_w, int x0, int y0, int x1,
                        int y1);
static void set_true(bool* arr, int h, int w, int y, int x);

void die(char* s) {
    fprintf(stderr, "%s\n", s);
    SDL_Quit();
    exit(1);
}

int main(int argc, char** argv) {
    if (argc != 5) {
        fprintf(stderr, "Wrong number of arguments. Usage: %s x0 y0 x1 y1\n",
                argv[0]);
        return 1;
    }

    /* Points of the line from arguments */
    const int x0 = atoi(argv[1]);
    const int y0 = atoi(argv[2]);
    const int x1 = atoi(argv[3]);
    const int y1 = atoi(argv[4]);

    /* Deltas */
    const int dx = (x1 - x0);
    const int dy = (y1 - y0);

    /* Length */
    const float len = sqrtf(dx * dx + dy * dy);
    printf("Length: %.2f\n", len);

    /* Angle */
    const float ang = atan2f(dy, dx);
    printf("Angle: %.2fº (%.2f rad)\n", RAD2DEG(ang), ang);

    /* Size of the graph will be the farthest point in each coordinate plus a
     * margin times 2 (positive and negative axis) */
    const int arr_h = (MAX(ABS(y0), ABS(y1)) + GRAPH_MARGIN) * 2 + 1;
    const int arr_w = (MAX(ABS(x0), ABS(x1)) + GRAPH_MARGIN) * 2 + 1;
    bool* arr       = calloc(arr_h * arr_w, sizeof(bool));

    bresenham_line(arr, arr_h, arr_w, x0, y0, x1, y1);

    /*------------------------------------------------------------------------*/
    /* SDL */

    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0)
        die("Unable to start SDL");

    SDL_Window* sdl_window =
      SDL_CreateWindow("Bresenham's line algorithm", SDL_WINDOWPOS_CENTERED,
                       SDL_WINDOWPOS_CENTERED, arr_w, arr_h, 0);
    if (!sdl_window)
        die("Error creating a window");

    Uint32 render_flags = SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC;
    SDL_Renderer* sdl_renderer =
      SDL_CreateRenderer(sdl_window, -1, render_flags);
    if (!sdl_renderer) {
        SDL_DestroyWindow(sdl_window);
        die("Error creating a renderer");
    }

    puts("Press <Esc> or <q> to exit...");

    /* Clear screen */
    SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 255);
    SDL_RenderClear(sdl_renderer);

    /* For key events */
    SDL_Event sdl_event;

    /* Current pixel */
    SDL_Rect px = {
        .w = 1,
        .h = 1,
    };

    bool sdl_loop = true;
    while (sdl_loop) {
        // Events
        while (SDL_PollEvent(&sdl_event)) {
            switch (sdl_event.type) {
                case SDL_QUIT:
                    sdl_loop = false;
                    break;
                case SDL_KEYDOWN:
                    /* Check the pressed key */
                    switch (sdl_event.key.keysym.scancode) {
                        case SDL_SCANCODE_ESCAPE:
                        case SDL_SCANCODE_Q:
                            sdl_loop = false;
                            break;
                        default:
                            break;
                    }
                    break;
                default:
                    break;
            }
        }

#ifdef DRAW_AXIS
        /* Draw Y axis */
        SDL_SetRenderDrawColor(sdl_renderer, 10, 10, 200, 255);
        SDL_RenderDrawLine(sdl_renderer, arr_w / 2, 0, arr_w / 2, arr_h);

        /* Draw X axis */
        SDL_SetRenderDrawColor(sdl_renderer, 200, 10, 10, 255);
        SDL_RenderDrawLine(sdl_renderer, 0, arr_h / 2, arr_w, arr_h / 2);
#endif

        /* Iterate line array */
        for (px.y = 0; px.y < arr_h; px.y++) {
            for (px.x = 0; px.x < arr_w; px.x++) {
                if (arr[px.y * arr_w + px.x]) {
                    SDL_SetRenderDrawColor(sdl_renderer, 200, 200, 200, 255);
                    SDL_RenderFillRect(sdl_renderer, &px);
                }
            }
        }

        /* Send to renderer */
        SDL_RenderPresent(sdl_renderer);

        SDL_Delay(1000 / FPS);
    }

    free(arr);
    return 0;
}

static void bresenham_line(bool* arr, int arr_h, int arr_w, int x0, int y0,
                           int x1, int y1) {
    if (ABS(y1 - y0) < ABS(x1 - x0)) {
        if (x1 >= x0)
            bresenham_x(arr, arr_h, arr_w, x0, y0, x1, y1);
        else
            bresenham_x(arr, arr_h, arr_w, x1, y1, x0, y0);
    } else {
        if (y1 >= y0)
            bresenham_y(arr, arr_h, arr_w, x0, y0, x1, y1);
        else
            bresenham_y(arr, arr_h, arr_w, x1, y1, x0, y0);
    }
}

static void bresenham_x(bool* arr, int arr_h, int arr_w, int x0, int y0, int x1,
                        int y1) {
    int dx = x1 - x0;
    int dy = (y1 > y0) ? y1 - y0 : y0 - y1;

    int diff   = 2 * dy - dx;
    int y_step = (y1 > y0) ? 1 : -1;

    int y = y0;
    for (int x = x0; x < x1; x++) {
        set_true(arr, arr_h, arr_w, y, x);

        if (diff > 0) {
            diff += 2 * (dy - dx);
            y += y_step;
        } else {
            diff += 2 * dy;
        }
    }
}

static void bresenham_y(bool* arr, int arr_h, int arr_w, int x0, int y0, int x1,
                        int y1) {
    int dx = (x1 > x0) ? x1 - x0 : x0 - x1;
    int dy = y1 - y0;

    int diff   = 2 * dx - dy;
    int x_step = (x1 > x0) ? 1 : -1;

    int x = x0;
    for (int y = y0; y < y1; y++) {
        set_true(arr, arr_h, arr_w, y, x);

        if (diff > 0) {
            diff += 2 * (dx - dy);
            x += x_step;
        } else {
            diff += 2 * dx;
        }
    }
}

static inline void set_true(bool* arr, int arr_h, int arr_w, int y, int x) {
    const int real_y = (arr_h / 2) - GRAPH_MARGIN - y;
    const int real_x = (arr_w / 2) + x;

    arr[real_y * arr_w + real_x] = true;
}

