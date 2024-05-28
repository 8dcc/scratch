/* Kata: https://www.codewars.com/kata/55a2d7ebe362935a210000b2 */

#include <stddef.h>

int find_smallest_int(int *vec, size_t len)
{
    int m = *vec;
    for (unsigned long n = 0; n < len; n++) {
        if (vec[n] < m) m = vec[n];
    }
    return m;
}
