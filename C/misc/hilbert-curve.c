
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <math.h> /* sqrt */

/*
 * Enumerator with all possible directions/orientations of a Hilbert curve.
 */
enum EDirection {
    DIR_UP,
    DIR_DOWN,
    DIR_LEFT,
    DIR_RIGHT,
};

/*
 * Structure representing the context needed to draw a Hilbert curve.
 */
typedef struct {
    /* Output grid for drawing */
    char* grid;

    /* Side (not size) of the output grid */
    size_t square_side;

    /* Current position in "blocks" or "Hilbert points" in the 'grid' */
    size_t x, y;

    /* Side (not size) of a block (i.e. Hilbert curve point) when drawing */
    size_t block_side;

    /* String where the characters will be read from */
    const char* input;

    /* Position in the input buffer, in characters */
    size_t input_pos;
} HilbertCtx;

/*----------------------------------------------------------------------------*/

/*
 * Draw the next input character into the output grid in the current position of
 * the 'HilbertCtx' structure.
 */
void draw_next(HilbertCtx* ctx) {
    const size_t base_y = ctx->y * ctx->block_side;
    const size_t base_x = ctx->x * ctx->block_side;
    for (size_t y = 0; y < ctx->block_side; y++) {
        for (size_t x = 0; x < ctx->block_side; x++) {
            const size_t raw_y = base_y + y;
            const size_t raw_x = base_x + x;

            assert(raw_y < ctx->square_side && raw_x < ctx->square_side);
            ctx->grid[ctx->square_side * raw_y + raw_x] =
              ctx->input[ctx->input_pos++];
        }
    }
}

/*
 * Move the coordinates in the 'HilbertCtx' to the specified direction.
 */
void move(HilbertCtx* ctx, enum EDirection direction) {
    switch (direction) {
        case DIR_UP:
            assert(ctx->y > 0);
            ctx->y--;
            break;
        case DIR_DOWN:
            assert(ctx->y + 1 < ctx->square_side);
            ctx->y++;
            break;
        case DIR_LEFT:
            assert(ctx->x > 0);
            ctx->x--;
            break;
        case DIR_RIGHT:
            assert(ctx->x + 1 < ctx->square_side);
            ctx->x++;
            break;
    }
}

/*
 * Generate a hilbert curve with the specified recursion level and orientation
 * in the current position according to the 'HilbertCtx'. The input and output
 * pointers are also obtained from the 'HilbertCtx'.
 */
void recursive_hilbert(HilbertCtx* ctx, int level, enum EDirection direction) {
    if (level <= 1) {
        /*
         * Last recursive level, draw the simplest form:
         *
         *   o      o
         *   |      |
         *   |      |
         *   o------o
         */
        switch (direction) {
            case DIR_UP:
                draw_next(ctx);
                move(ctx, DIR_DOWN);
                draw_next(ctx);
                move(ctx, DIR_RIGHT);
                draw_next(ctx);
                move(ctx, DIR_UP);
                draw_next(ctx);
                break;
            case DIR_DOWN:
                draw_next(ctx);
                move(ctx, DIR_UP);
                draw_next(ctx);
                move(ctx, DIR_LEFT);
                draw_next(ctx);
                move(ctx, DIR_DOWN);
                draw_next(ctx);
                break;
            case DIR_LEFT:
                draw_next(ctx);
                move(ctx, DIR_RIGHT);
                draw_next(ctx);
                move(ctx, DIR_DOWN);
                draw_next(ctx);
                move(ctx, DIR_LEFT);
                draw_next(ctx);
                break;
            case DIR_RIGHT:
                draw_next(ctx);
                move(ctx, DIR_LEFT);
                draw_next(ctx);
                move(ctx, DIR_UP);
                draw_next(ctx);
                move(ctx, DIR_RIGHT);
                draw_next(ctx);
                break;
        }
    } else {
        /*
         * We are not in the last recursive level; draw the same shape, but
         * calling ourselves recursively each time.
         *
         *   [+]    [+]
         *    |      |
         *    |      |
         *   [+]----[+]
         *
         * Where each [+] represents a smaller Hilbert curve that is drawn with
         * a specific orientation.
         */
        switch (direction) {
            case DIR_UP:
                recursive_hilbert(ctx, level - 1, DIR_LEFT);
                move(ctx, DIR_DOWN);
                recursive_hilbert(ctx, level - 1, DIR_UP);
                move(ctx, DIR_RIGHT);
                recursive_hilbert(ctx, level - 1, DIR_UP);
                move(ctx, DIR_UP);
                recursive_hilbert(ctx, level - 1, DIR_RIGHT);
                break;
            case DIR_DOWN:
                recursive_hilbert(ctx, level - 1, DIR_RIGHT);
                move(ctx, DIR_UP);
                recursive_hilbert(ctx, level - 1, DIR_DOWN);
                move(ctx, DIR_LEFT);
                recursive_hilbert(ctx, level - 1, DIR_DOWN);
                move(ctx, DIR_DOWN);
                recursive_hilbert(ctx, level - 1, DIR_LEFT);
                break;
            case DIR_LEFT:
                recursive_hilbert(ctx, level - 1, DIR_UP);
                move(ctx, DIR_RIGHT);
                recursive_hilbert(ctx, level - 1, DIR_LEFT);
                move(ctx, DIR_DOWN);
                recursive_hilbert(ctx, level - 1, DIR_LEFT);
                move(ctx, DIR_LEFT);
                recursive_hilbert(ctx, level - 1, DIR_DOWN);
                break;
            case DIR_RIGHT:
                recursive_hilbert(ctx, level - 1, DIR_DOWN);
                move(ctx, DIR_LEFT);
                recursive_hilbert(ctx, level - 1, DIR_RIGHT);
                move(ctx, DIR_UP);
                recursive_hilbert(ctx, level - 1, DIR_RIGHT);
                move(ctx, DIR_RIGHT);
                recursive_hilbert(ctx, level - 1, DIR_UP);
                break;
        }
    }
}

/*
 * Transform the specified input string into an output grid buffer using a
 * space-filling Hilbert curve, with the specified recursion level.
 */
bool transform_hilbert(const char* input, char* output, int level) {
    assert(input != NULL && output != NULL);
    if (level <= 0) {
        fprintf(stderr, "Recursion level must be greater than zero.\n");
        return false;
    }

    const size_t input_sz = strlen(input);

    const double square_side_flt = sqrt(input_sz);
    const size_t square_side     = (size_t)square_side_flt;
    if (square_side_flt != square_side) {
        fprintf(stderr, "The input data cannot be squared.\n");
        return false;
    }

    /* Number of hilbert points per square side */
    const size_t draws_per_side = (size_t)pow(2, level);
    if (draws_per_side > square_side) {
        fprintf(stderr, "Not enough data for the specified hilbert level.\n");
        return false;
    }

    /* Size of each hilbert point */
    const size_t block_side = square_side / draws_per_side;

    HilbertCtx ctx = {
        .grid        = output,
        .square_side = square_side,
        .x           = 0,
        .y           = 0,
        .block_side  = block_side,
        .input       = input,
        .input_pos   = 0,
    };

    recursive_hilbert(&ctx, level, DIR_UP);
    return true;
}

int main(void) {
    const int h = 16, w = 16;
    char output[h * w];
    const char* input =
      "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF01234567"
      "89ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF"
      "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF01234567"
      "89ABCDEF0123456789ABCDEF0123456789ABCDEF";

    /* Fill the 'output' grid with the Hilbert curve of the 'input' */
    if (!transform_hilbert(input, output, 4))
        return 1;

    /* Plot the filled grid */
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < h; x++)
            putchar(output[w * y + x]);
        putchar('\n');
    }

    return 0;
}
