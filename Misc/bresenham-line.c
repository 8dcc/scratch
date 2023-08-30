/*
 * For more information on math stuff, see:
 *   RAD2DEG:   https://en.wikipedia.org/wiki/Radian
 *   Length:    https://en.wikipedia.org/wiki/Hypotenuse
 *   Angle:     https://en.wikipedia.org/wiki/Atan2
 *              https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
 *   Bresenham: https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define GRAPH_MARGIN 1

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
static inline void draw_arr(bool* arr, int h, int w);

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
    const int arr_h = (MAX(ABS(y0), ABS(y1)) + GRAPH_MARGIN) * 2;
    const int arr_w = (MAX(ABS(x0), ABS(x1)) + GRAPH_MARGIN) * 2;
    bool* arr       = calloc(arr_h * arr_w, sizeof(bool));

    bresenham_line(arr, arr_h, arr_w, x0, y0, x1, y1);
    draw_arr(arr, arr_h, arr_w);

    free(arr);
    return 0;
}

/* We need a wrapper because the base algorithm only covers lines with slope
 * from 0 to 1 (45º) */
static void bresenham_line(bool* arr, int arr_h, int arr_w, int x0, int y0,
                           int x1, int y1) {
    /* Slope is more horizontal than vertical, iterate X, otherwise Y */
    if (ABS(y1 - y0) < ABS(x1 - x0)) {
        if (x1 >= x0)
            /* Positive slope */
            bresenham_x(arr, arr_h, arr_w, x0, y0, x1, y1);
        else
            /* Negative slope, change argument order */
            bresenham_x(arr, arr_h, arr_w, x1, y1, x0, y0);
    } else {
        if (y1 >= y0)
            /* Positive slope */
            bresenham_y(arr, arr_h, arr_w, x0, y0, x1, y1);
        else
            /* Negative slope, change argument order */
            bresenham_y(arr, arr_h, arr_w, x1, y1, x0, y0);
    }
}

/* For iterating the X axis with positive and negative slope, degrees from +45
 * to -45 (or 135 to -135 if you change the order of the arguments):
 *
 * (+135º) .\   /. (+45º)
 *         ..\ /..
 *         ...X...
 *         ../ \..
 * (-135º) ./   \. (-45º)
 */
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

/* For iterating the Y axis with positive and negative slope, degrees from +45
 * to +135 (or -45 to -135 if you change the order of the arguments):
 *
 * (+135º) \...../ (+45º)
 *          \.../
 *           \./
 *            X
 *           /.\
 *          /...\
 * (-135º) /.....\ (-45º)
 */
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
    /* We subtract "y" for calculating "real_y" because (x0, y1) is 1 above the
     * center, and going up one row in a 2d array means subtracting one from
     * the Y coord. This does not happen with the X axis. */
    const int real_y = (arr_h / 2) - y;
    const int real_x = (arr_w / 2) + x;

    arr[real_y * arr_w + real_x] = true;
}

static void draw_arr(bool* arr, int h, int w) {
    /* Top border */
    putchar('+');
    for (int x = 0; x < w; x++)
        putchar('-');
    printf("+\n");

    for (int y = 0; y < h; y++) {
        /* Left border */
        putchar('|');

        for (int x = 0; x < w; x++) {
            if (arr[y * w + x])
                putchar('.');
            else
                putchar(' ');
        }

        /* Right border */
        printf("|\n");
    }

    /* Bottom border */
    putchar('+');
    for (int x = 0; x < w; x++)
        putchar('-');
    printf("+\n");
}
