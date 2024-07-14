
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h> /* strlen() */

#define MIN(A, B) (((A) < (B)) ? (A) : (B))
#define MAX(A, B) (((A) > (B)) ? (A) : (B))

static uint32_t hamming_distance(const char* a, const char* b) {
    const size_t a_sz = strlen(a);
    const size_t b_sz = strlen(b);

    /* Calculate which is the biggest and smallest string */
    const size_t min_sz = MIN(a_sz, b_sz);
    const size_t max_sz = MAX(a_sz, b_sz);

    /* All of the bits from A that are not in B (or viceversa) are part of the
     * hamming distance. */
    uint32_t result = (max_sz - min_sz) * 8;

    /* Only compare up to the length of the smallest string */
    for (size_t i = 0; i < min_sz; i++) {
        const uint8_t byte_a = a[i];
        const uint8_t byte_b = b[i];

        /* Iterate each bit */
        for (int j = 0; j < 8; j++) {
            const bool bit_a = (byte_a >> j) & 1;
            const bool bit_b = (byte_b >> j) & 1;

            /* Add one to the hamming distance if they don't match */
            if (bit_a != bit_b)
                result++;
        }
    }

    return result;
}

int main(void) {
    const char* str1 = "this is a test";
    const char* str2 = "wokka wokka!!!";
    printf("String 1: \"%s\"\n"
           "String 2: \"%s\"\n",
           str1, str2);

    const uint32_t distance = hamming_distance(str1, str2);
    printf("Hamming distance: %d\n", distance);

    return 0;
}
