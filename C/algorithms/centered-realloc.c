
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Matrix {
    int* data;
    int w, h;
} Matrix;

static void centered_realloc(Matrix* matrix, int new_w, int new_h) {
    const int old_w = matrix->w;
    const int old_h = matrix->h;

    int x_offset = 0;
    int y_offset = 0;

    /* Calculate difference between old width and height. If it's positive,
     * calculate the offsets for centering, and update the values in the
     * Matrix. */
    const int w_diff = new_w - old_w;
    if (w_diff > 0) {
        x_offset  = w_diff / 2;
        matrix->w = new_w;
    }

    const int h_diff = new_h - old_h;
    if (h_diff > 0) {
        y_offset  = h_diff / 2;
        matrix->h = new_h;
    }

    /* Width and height probably changed, recalculate total size and allocate
     * new array. */
    const size_t new_sz = matrix->w * matrix->h;
    int* new_data       = calloc(new_sz, sizeof(int));

    /* Copy the old array, adding the X and Y offsets */
    for (int y = 0; y < old_h; y++) {
        for (int x = 0; x < old_w; x++) {
            const int new_idx = matrix->w * (y + y_offset) + (x + x_offset);
            const int old_idx = old_w * y + x;
            new_data[new_idx] = matrix->data[old_idx];
        }
    }

    /* Update the `data' pointer in the Matrix */
    free(matrix->data);
    matrix->data = new_data;
}

/*----------------------------------------------------------------------------*/

static void matrix_print(Matrix* matrix) {
    for (int y = 0; y < matrix->h; y++) {
        printf("[ ");
        for (int x = 0; x < matrix->w; x++) {
            const int num = matrix->data[matrix->w * y + x];
            if (num > 0)
                printf("%d ", num);
            else
                printf(". ");
        }
        printf("]\n");
    }
    putchar('\n');
}

static Matrix* matrix_create(void) {
    Matrix* matrix = malloc(sizeof(Matrix));
    matrix->w      = 4;
    matrix->h      = 4;
    matrix->data   = calloc(matrix->w * matrix->h, sizeof(int));

    /* Fill with initial data */
    for (int y = 0; y < matrix->h; y++)
        for (int x = 0; x < matrix->w; x++)
            matrix->data[matrix->w * y + x] = x + 1;

    return matrix;
}

static void matrix_free(Matrix* matrix) {
    free(matrix->data);
    free(matrix);
}

int main(void) {
    Matrix* matrix;

    /* Print initial test matrix */
    matrix = matrix_create();
    matrix_print(matrix);

    /* Realloc and print with different sizes. Re-create the matrix because we
     * don't want to center the previous realloc'd */
    centered_realloc(matrix, 5, 5);
    matrix_print(matrix);
    matrix_free(matrix);

    matrix = matrix_create();
    centered_realloc(matrix, 6, 6);
    matrix_print(matrix);
    matrix_free(matrix);

    matrix = matrix_create();
    centered_realloc(matrix, 10, 15);
    matrix_print(matrix);
    matrix_free(matrix);

    return 0;
}
