/* Kata: https://www.codewars.com/kata/52e88b39ffb6ac53a400022e */

#include <inttypes.h>
#include <stdio.h>

void uint32_to_ip(uint32_t number, char* IPv4) {
    uint8_t arr[4];

    for (int i = 3; i >= 0; i--) {
        arr[i] = number & 255;
        number >>= 8;
    }

    sprintf(IPv4, "%d.%d.%d.%d", arr[0], arr[1], arr[2], arr[3]);
}
