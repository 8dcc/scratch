#include <stddef.h>

static inline unsigned int digits(int n) {
    if (n < 0)
        n = -n;

    int i;
    for (i = 1; n >= 10; i++, n /= 10)
        ;
    return i;
}

int find_longest(int *numbers, size_t numbers_size) {
    int ret = 0;
    int max_digits = 0;

    for (int i = 0; i < numbers_size; i++) {
        const int cur_digits = digits(numbers[i]);
        if (cur_digits > max_digits) {
            ret = numbers[i];
            max_digits = cur_digits;
        }
    }

    return ret;
}

