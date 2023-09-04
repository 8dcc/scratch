/*
 * Compile with:
 * gcc -Ofast -o pgm-mandelbrot.out pgm-mandelbrot.c
 *
 * Run with:
 * ./pgm-mandelbrot.out <width> <height> > FILE.pgm
 *
 * On .pgm formats, see:
 * https://en.wikipedia.org/wiki/Netpbm#File_formats
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

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

    if (w < 1 || h < 1 || max_iter < 1) {
        fprintf(stderr,
                "Wrong width, height or max_iter values.\n"
                "Usage: %s width height max_iter > file.pgm\n",
                argv[0]);
        return 1;
    }

    /* .PGM header */
    printf("P2\n"
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

            /* Iterations will be used for grayscale color */
            printf("%d ", iter);
        }

        /* Not needed */
        putchar('\n');
    }

    return 0;
}
