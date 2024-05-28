/* Kata: https://www.codewars.com/kata/58f8a3a27a5c28d92e000144 */

#include <stdbool.h>

bool firstNonConsecutive (const int arr[], const int length, int *first) {
    for (int i = 1; i < length; i++) {
        if (arr[i] != arr[i-1] + 1) {
            *first = arr[i];
            return true;
        }
    }
    return false;
}

#ifdef DIFFERENT_STEPS
#include <stdlib.h>

#define STEP(a, b) \
    { abs(b) - abs(a); }

bool firstNonConsecutive(const int arr[], const int length, int *first) {
    int first_step = STEP(arr[0], arr[1]);
    for (int i = 0; i < length; i++) {
        if (/*TODO*/) {
            *first = arr[n];
            return true;
        }
    }
    return false;
}
#endif /* DIFFERENT_STEPS */

