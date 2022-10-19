/* 3-01 */

#include <stdio.h>

#define LENGTH(arr) sizeof(arr) / sizeof(arr[0])

// Finds x in arr, where arr is ordered increasingly.
int binsearch(int target, int arr[], int arr_len);

int main() {
    int arr[] = { 5, 6, 7, 8, 9, 14, 16, 22, 30, 33 };

    int r = binsearch(8, arr, LENGTH(arr));
    printf("Result: %d\n", r);

    return 0;
}

int binsearch(int target, int arr[], int arr_len) {
    int low = 0, mid = 0, high = arr_len - 1;

    while (low <= high && target != arr[mid]) {
        if (target > arr[mid])
            low = mid + 1;    // If the target is in the upper half, or in the
                              // middle, change the starting pos to the middle.
        else
            high = mid + 1;    // If the target is in the lower half, reduce the arr
                               // in half
        mid = (low + high) / 2;    // High + 'low offset' / 2

    }

    if (target == arr[mid])
        return mid;
    else
        return -1;
}
