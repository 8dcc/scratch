
#include <stdio.h>

#define R 4    // Rows
#define C 6    // Cols

// Colors
#define C_B "\x1B[1;37m"
#define C_N "\x1B[0m"

static void print_arr(int* arr);

int main() {
    int arr[R][C] = {
        { 0, 1, 2, 3, 4, 5 },
        { 6, 7, 8, 9, 10, 11 },
        { 12, 13, 14, 15, 16, 17 },
        { 18, 19, 20, 21, 22, 23 },
    };
    int* arr_p = &arr[0][0];

    printf("Original:\n"
           "{  0,  1,  2,  3,  4,  5 },\n"
           "{  6,  7,  8,  9, 10, 11 },\n"
           "{ 12, 13, 14, 15, 16, 17 },\n"
           "{ 18, 19, 20, 21, 22, 23 },\n"
           "\n");
    print_arr(arr_p);

    return 0;
}

void print_arr(int* arr) {
    for (int y = 0; y < R; y++) {
        for (int x = 0; x < C; x++)
            printf("%d:%d:" C_B "%2d " C_N, y, x, arr[C * y + x]);

        putchar('\n');
    }
}
