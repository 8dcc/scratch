
#include <stdio.h>

#define H 4
#define W 6

static void print_arr(int* arr) {
    for (int y = 0; y < H; y++) {
        for (int x = 0; x < W; x++)
            printf("%d:%d:%02d ", y, x, arr[W * y + x]);

        putchar('\n');
    }
}

int main(void) {
    int arr[H][W] = {
        { 0, 1, 2, 3, 4, 5 },
        { 6, 7, 8, 9, 10, 11 },
        { 12, 13, 14, 15, 16, 17 },
        { 18, 19, 20, 21, 22, 23 },
    };

    printf("Original:\n"
           "{  0,  1,  2,  3,  4,  5 },\n"
           "{  6,  7,  8,  9, 10, 11 },\n"
           "{ 12, 13, 14, 15, 16, 17 },\n"
           "{ 18, 19, 20, 21, 22, 23 },\n\n");

    int* arr_p = &arr[0][0];
    print_arr(arr_p);

    return 0;
}
