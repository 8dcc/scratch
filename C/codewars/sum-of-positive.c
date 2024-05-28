/* Kata: https://www.codewars.com/kata/5715eaedb436cf5606000381 */

#include <stddef.h>

int positive_sum(const int *values, size_t count) {
    int r = 0;
    for (int n = 0; n < count; n++) {
        if (values[n] > 0) r += values[n];
    }
    return r;
}
