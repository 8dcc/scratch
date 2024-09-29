/*
 * NOTE: The code in my fs-os project might be more updated.
 *
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
 *
 * On mandelbrot, see:
 * - https://en.wikipedia.org/wiki/Mandelbrot_set
 * - http://warp.povusers.org/Mandelbrot/
 *   https://web.archive.org/web/20230324155752/http://warp.povusers.org/Mandelbrot/
 *
 * Interesting parameters:
 *   zoom:0.1       x: -0.7         y: -0.11
 *   zoom:0.005     x: -0.74        y: -0.11
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h> /* hsv2rgb */

/* Max HSV values (Used for scaling) */
#define MAX_H 360
#define MAX_S 1.0
#define MAX_V 1.0

/* Default HSV values */
#define COL_H 360
#define COL_S 1.0
#define COL_V 1.0

/* Smaller -> More zoom (1.0 is default) */
#define ZOOM 1.0

/* Offsets for moving in the set. */
#define X_OFFSET 0.0 /* -2.0..+2.0 (Left..Right) */
#define Y_OFFSET 0.0 /* -1.0..+1.0 (Up..Down) */

/* RGB color used for drawing the interior of the mandelbrot set */
#define INSIDE_COL "0 0 0 "

static void hsv2rgb(float* r, float* g, float* b, float h, float s, float v);

int main(int argc, char** argv) {
    if (argc != 4) {
        fprintf(stderr,
                "Wrong number of arguments.\n"
                "Usage: %s width height max_iter > file.pgm\n",
                argv[0]);
        return 1;
    }

    const int w = atoi(argv[1]);
    const int h = atoi(argv[2]);

    /* Higher means more precision */
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

    /* Calculate some values here for performance */
    const double scaled_h = h / 2.0;
    const double scaled_w = w / 3.0;

    /* Mandelbrot */
    for (int y_px = 0; y_px < h; y_px++) {
        /* Real Y is the mandelbrot center vertically. We subtract 1.0 (half the
         * height) to center it vertically. */
        double real_y = (y_px / scaled_h) - 1.0 ;
        real_y *= ZOOM;
        real_y += Y_OFFSET;

        for (int x_px = 0; x_px < w; x_px++) {
            /* Real X is the mandelbrot center horizontally. We subtract 2.0
             * (half the width) to center it horizontally. */
            double real_x = (x_px / scaled_w) - 2.0;
            real_x *= ZOOM;
            real_x += X_OFFSET;

            /* These 2 values will be increased each iteration below */
            double x = real_x;
            double y = real_y;

            bool inside_set = true;

            /* In each iteration, we will check if we are inside the mandebrot
             * set. An interesting property of the mandelbrot set is that the
             * more iterations an outside value takes, the closer to the set is.
             * We can use this to change colors. Ouside of the loop. */
            int iter;
            for (iter = 0; iter < max_iter; iter++) {
                /* Calulate squares once */
                double sqr_x = x * x;
                double sqr_y = y * y;

                /* Absolute value of a complex number is the distance from
                 * origin: sqrt(x^2 + y^2) > 2 */
                if ((sqr_x + sqr_y) > 2 * 2) {
                    inside_set = false;
                    break;
                }

                /* This part is explained in the povusers.org link on credits */
                y = (2.0 * x * y) + real_y;
                x = (sqr_x - sqr_y) + real_x;
            }

            /* If it's inside the set, draw fixed color */
            if (inside_set) {
                printf("%s", INSIDE_COL);
                continue;
            }

            /* Get 0..360 hue for color based on iter..max_iter
             *
             * NOTE: Make sure you change the variable type to double if you
             * want to scale the Saturation or Value instead (to not truncate to
             * zero) */
            int scaled_hue = iter * COL_H / max_iter;

            float r, g, b;
            hsv2rgb(&r, &g, &b, scaled_hue, COL_S, COL_V);

            /* NOTE: Try to replace some of these parameters with zeros or mess
             * with the scaling and see what happens :) */
            int fr = r * 255.f;
            int fg = g * 255.f;
            int fb = b * 255.f;
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
