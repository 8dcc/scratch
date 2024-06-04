
#include <stdio.h>

#define LENGTH(x) (sizeof(x) / sizeof((x)[0]))

int main() {
    int arr[] = { 1, 2, 3, 4, 5, 6, 7 };

    printf("Length of array: %ld\n", LENGTH(arr));

    return 0;
}
