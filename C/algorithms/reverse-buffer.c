
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Reverse a buffer in-place, with the specified number of elements of the
 * specified size each. Returns the 'buf' argument on success, or NULL on error.
 */
static void* reverse_buffer(void* buf, size_t num_elems, size_t elem_sz) {
    uint8_t* casted             = (uint8_t*)buf;
    const size_t half_num_elems = num_elems / 2;

    /* Temporary buffer for swapping values */
    uint8_t* tmp = malloc(elem_sz);
    if (tmp == NULL)
        return NULL;

    /*
     * Go from 'buf[0]' to 'buf[size/2]' and replace each byte with the byte
     * from the opposite end:
     *
     *     s = [ H E L L O ] => [ O L L E H ]
     *           | |   | |
     *           | ----- |
     *           |-------|
     */
    for (size_t i = 0; i < half_num_elems; i++) {
        const size_t raw_start_idx = i * elem_sz;
        const size_t raw_end_idx   = (num_elems - 1 - i) * elem_sz;

        memcpy(tmp, &casted[raw_start_idx], elem_sz);
        memcpy(&casted[raw_start_idx], &casted[raw_end_idx], elem_sz);
        memcpy(&casted[raw_end_idx], tmp, elem_sz);
    }

    free(tmp);
    return buf;
}

/*----------------------------------------------------------------------------*/

#define BUFLEN(BUF) (sizeof(BUF) / sizeof((BUF)[0]))

#define TEST_STR(STR)                                                          \
    do {                                                                       \
        printf("Original: '%s'\n", STR);                                       \
        assert(reverse_buffer(STR, sizeof(STR) - 1, sizeof(char)) != NULL);    \
        printf("Reversed: '%s'\n", STR);                                       \
    } while (0)

#define TEST_BUF(BUF)                                                          \
    do {                                                                       \
        printf("Original: ");                                                  \
        for (size_t i = 0; i < BUFLEN(BUF); i++)                               \
            printf("%02lX ", (BUF)[i]);                                        \
        putchar('\n');                                                         \
        assert(reverse_buffer(BUF, BUFLEN(BUF), sizeof((BUF)[0])) != NULL);    \
        printf("Reversed: ");                                                  \
        for (size_t i = 0; i < BUFLEN(BUF); i++)                               \
            printf("%02lX ", (BUF)[i]);                                        \
        putchar('\n');                                                         \
    } while (0)

int main(void) {
    char str1[] = "Hello";                    /* Odd length */
    char str2[] = "Hello!";                   /* Even length */
    long buf1[] = { 10, 20, 30, 40, 50 };     /* Odd length */
    long buf2[] = { 10, 20, 30, 40, 50, 60 }; /* Even length */

    TEST_STR(str1);
    putchar('\n');
    TEST_STR(str2);
    putchar('\n');
    TEST_BUF(buf1);
    putchar('\n');
    TEST_BUF(buf2);

    return 0;
}
