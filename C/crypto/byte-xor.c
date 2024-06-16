/* https://cryptopals.com/sets/1/challenges/2 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*----------------------------------------------------------------------------*/
/* Misc util functions */

static bool hex2bytes(uint8_t* bytes, const char* str) {
    /* Each iteration, parse 2 characters of the input */
    for (int i = 0; *str != '\0'; i++) {
        uint8_t n = 0;

        if (*str >= '0' && *str <= '9')
            n = *str - '0';
        else if (*str >= 'a' && *str <= 'f')
            n = 10 + *str - 'a';
        else if (*str >= 'A' && *str <= 'F')
            n = 10 + *str - 'A';
        else
            return false;

        bytes[i] = (n & 0xF) << 4;
        str++;

        if (*str >= '0' && *str <= '9')
            n = *str - '0';
        else if (*str >= 'a' && *str <= 'f')
            n = 10 + *str - 'a';
        else if (*str >= 'A' && *str <= 'F')
            n = 10 + *str - 'A';
        else
            return false;

        bytes[i] |= n & 0xF;
        str++;
    }

    return true;
}

static void print_bytes(uint8_t* bytes, size_t sz) {
    while (sz-- > 0)
        printf("%02X", *bytes++);
    putchar('\n');
}

/*----------------------------------------------------------------------------*/
/* String XOR functions */

static uint8_t* arr_xor(uint8_t* a, uint8_t* b, size_t sz) {
    uint8_t* result = malloc(sz);

    for (size_t i = 0; i < sz; i++)
        result[i] = a[i] ^ b[i];

    return result;
}

/*----------------------------------------------------------------------------*/
/* Main function */

int main(int argc, char** argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <str1> <str2>\n", argv[0]);
        return 1;
    }

    const size_t len1 = strlen(argv[1]);
    const size_t len2 = strlen(argv[2]);
    if (len1 != len2) {
        fprintf(stderr, "Expected strings of same length\n");
        return 1;
    }

    size_t bytes_sz  = len1 / 2;
    uint8_t* bytes1 = malloc(bytes_sz);
    uint8_t* bytes2 = malloc(bytes_sz);
    if (!hex2bytes(bytes1, argv[1]) || !hex2bytes(bytes2, argv[2])) {
        fprintf(stderr, "Expected a string in hexadecimal format\n");
        return 1;
    }

    uint8_t* result = arr_xor(bytes1, bytes2, bytes_sz);
    print_bytes(result, bytes_sz);

    free(result);
    free(bytes2);
    free(bytes1);
    return 0;
}
