/* Kata: https://www.codewars.com/kata/5899dc03bc95b1bf1b0000ad */

#include <stddef.h>

void invert(int *values, size_t values_size)
{
    for (int n = 0; n < values_size; n++) {
        values[n] = -values[n];
    }
}
