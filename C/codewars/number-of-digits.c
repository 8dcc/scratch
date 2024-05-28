/* Kata: https://www.codewars.com/kata/58fa273ca6d84c158e000052 */

#include <stdint.h>

unsigned digits(uint64_t n) {
    int i;
    for (i = 1; n >= 10; i++, n /= 10)
        ;
    return i;
}
