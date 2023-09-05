/*
 * Same as pgm version but with color.
 *
 * Compile with:
 * gcc -Ofast -o ppm-mandelbrot.out ppm-mandelbrot.c -lm
 *
 * Run with:
 * ./ppm-mandelbrot.out <width> <height> > FILE.pgm
 *
 * On .ppm formats, see:
 * https://en.wikipedia.org/wiki/Netpbm#File_formats
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h> /* hsv2rgb */

/* Max hue. From 0 to 360 */
#define MAX_H 340

/* Max saturation and value for HSV */
#define COL_S 1.0f
#define COL_V 1.0f

static void hsv2rgb(float* r, float* g, float* b, float h, float s, float v);

int main(int argc, char** argv) {
    if (argc != 4) {
        fprintf(stderr,
                "Wrong number of arguments.\n"
                "Usage: %s width height max_iter > file.pgm\n",
                argv[0]);
        return 1;
    }

    int w = atoi(argv[1]);
    int h = atoi(argv[2]);

    /* Each iteration will be the gray color. Higher means more precision */
    const int max_iter = atoi(argv[3]);

    if (w < 1 || h < 1 || max_iter < 1 || max_iter > 255) {
        fprintf(stderr,
                "Wrong width, height or max_iter values.\n"
                "Usage: %s width height max_iter > file.pgm\n",
                argv[0]);
        return 1;
    }

    /* .PPM header */
    printf("P3\n"
           "%d %d\n"
           "%d\n",
           w, h, max_iter);

    /* Mandelbrot */
    for (int y_px = 0; y_px < h; y_px++) {
        double real_y = (y_px / (h / 2.0)) - 1.0;

        for (int x_px = 0; x_px < w; x_px++) {
            double real_x = (x_px / (w / 3.0)) - 2.0;

            double x = real_x;
            double y = real_y;

            int iter = 0;
            while (iter < max_iter && (x * x + y * y) <= 2 * 2) {
                double tmp_x = (x * x - y * y) + real_x;
                y            = (2.0 * x * y) + real_y;
                x            = tmp_x;

                iter++;
            }

            /* Get 0..360 hue for color based on iter..max_iter */
            int hue_scale = iter * MAX_H / max_iter;

            float r, g, b;
            hsv2rgb(&r, &g, &b, hue_scale, COL_S, COL_V);

            int fr = r * 255.f;
            int fg = g * 255.f;
            int fb = b * 255.f;

            /* NOTE: Try to replace some of these fr's with zeros and see what
             * happens :)*/
            printf("%d %d %d ", fr, fg, fb);
        }

        /* Not needed */
        putchar('\n');
    }

    return 0;
}

/* https://gist.github.com/8dcc/5f559419bb1f27eb22ea5b9da0343b1b */
static void hsv2rgb(float* r, float* g, float* b, float h, float s, float v) {
    float chroma = v * s; /* Chroma */
    float prime  = fmod(h / 60.f, 6);
    float x      = chroma * (1 - fabs(fmod(prime, 2) - 1));
    float m      = v - chroma;

    if (prime >= 0 && prime < 1) {
        *r = chroma;
        *g = x;
        *b = 0;
    } else if (prime >= 1 && prime < 2) {
        *r = x;
        *g = chroma;
        *b = 0;
    } else if (prime >= 2 && prime < 3) {
        *r = 0;
        *g = chroma;
        *b = x;
    } else if (prime >= 3 && prime < 4) {
        *r = 0;
        *g = x;
        *b = chroma;
    } else if (prime >= 4 && prime < 5) {
        *r = x;
        *g = 0;
        *b = chroma;
    } else if (prime >= 5 && prime < 6) {
        *r = chroma;
        *g = 0;
        *b = x;
    } else {
        *r = 0;
        *g = 0;
        *b = 0;
    }

    *r += m;
    *g += m;
    *b += m;
}
