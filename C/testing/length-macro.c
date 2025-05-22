
#include <stdio.h>

#define LENGTH(x) (sizeof(x) / sizeof((x)[0]))

int main(void) {
    int arr[] = { 1, 2, 3, 4, 5, 6, 7 };
    printf("Length of array: %zu\n", LENGTH(arr));

    return 0;
}
