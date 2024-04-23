/* Used in https://github.com/8dcc/libsigscan */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Used for getting the bytes from IDA patterns.
 * Converts: "E0" -> 224 */
static uint8_t libsigscan_hex2byte(const char* hex) {
    int ret = 0;

    /* Skip leading spaces, if any */
    while (*hex == ' ')
        hex++;

    /* Store a byte (two digits of string) */
    for (int i = 0; i < 2 && hex[i] != '\0'; i++) {
        char c = hex[i];

        /* For example "E ", although the format should always be "0E" */
        if (c == ' ')
            break;

        uint8_t n = 0;
        if (c >= '0' && c <= '9')
            n = c - '0';
        else if (c >= 'a' && c <= 'f')
            n = 10 + c - 'a';
        else if (c >= 'A' && c <= 'F')
            n = 10 + c - 'A';

        /* Shift size of 0xF and add the next half of byte */
        ret <<= 4;
        ret |= n & 0xF;
    }

    return ret & 0xFF;
}

/*
 * Convert `ida' signature to code + mask format. Allocate `code_ptr' and
 * `mask_ptr'.
 *
 * IDA format:  "FF ? ? 89"
 * Code format: "\xFF\x00\x00\x89"
 * Mask format: "x??x"
 */
static void libsigscan_ida2code(const char* ida, uint8_t** code_ptr,
                                char** mask_ptr) {
    int arr_sz    = 100;
    uint8_t* code = *code_ptr = malloc(arr_sz);
    char* mask = *mask_ptr = malloc(arr_sz);

    /* Skip preceding spaces from pattern, if any */
    while (*ida == ' ')
        ida++;

    int i;
    for (i = 0; *ida != '\0'; i++) {
        /* If the output arrays are full, reallocate. The `arr_sz' variable is
         * used for both `code' and `mask' arrays. */
        if (i >= arr_sz) {
            arr_sz += 100;
            code = *code_ptr = realloc(code, arr_sz);
            mask = *mask_ptr = realloc(mask, arr_sz);
        }

        if (*ida == '?') {
            code[i] = 0x00;
            mask[i] = '?';

            /* "A1 ?? ?? B2" -> "A1 ? ? B2" */
            while (*ida == '?')
                ida++;
        } else {
            /* Convert "E0" into 224 */
            code[i] = libsigscan_hex2byte(ida);
            mask[i] = 'x';

            /* Go to next byte separator in pattern (space) */
            while (*ida != ' ' && *ida != '\0')
                ida++;
        }

        /* Skip trailing spaces */
        while (*ida == ' ')
            ida++;
    }

    if (i >= arr_sz)
        mask = *mask_ptr = realloc(mask, arr_sz + 1);

    /* Indicate the end of the pattern in the mask, since 0x00 is valid in
     * code[] */
    mask[i] = '\0';
}

int main(void) {
    const char* ida = "89 04 24 FF 92 ? ? ? ? 89 34 24";

    uint8_t* code;
    char* mask;
    libsigscan_ida2code(ida, &code, &mask);

    printf("Original: \"%s\"\n", ida);

    printf("Code:      ");
    for (int i = 0; mask[i] != '\0'; i++)
        printf("%02X ", code[i]);
    putchar('\n');

    printf("Mask:      ");
    for (int i = 0; mask[i] != '\0'; i++)
        printf("%c", mask[i]);
    putchar('\n');

    printf("Masked:    ");
    for (int i = 0; mask[i] != '\0'; i++) {
        if (mask[i] == '?')
            printf("? ");
        else
            printf("%02X ", code[i]);
    }
    putchar('\n');

    return 0;
}
