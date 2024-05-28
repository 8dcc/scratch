/* Kata: https://www.codewars.com/kata/5a00e05cc374cb34d100000d */

#include <stdint.h>
#include <stdlib.h>

uint16_t* reverse_seq(uint16_t num) {
    if (!num)
        return NULL;

    uint16_t* arr = calloc(num, sizeof(uint16_t));

    for (int i = 0; i < num; i++)
        arr[i] = num - i;

    return arr;
}
