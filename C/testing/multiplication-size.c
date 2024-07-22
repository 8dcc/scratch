
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    /*
     * These 32-bit variables can hold up to UINT32_MAX. If we multiply these
     * variables, the resulting expression is of type `uint32_t'.
     *
     * Since the result of the multiplication can be bigger than UINT32_MAX, it
     * will get "truncated" to 32-bits.
     */
    const uint32_t width  = 0xDEADBEEF;
    const uint32_t height = 0x100;

    /* Even though the destination variable is 64-bits, the result of the
     * multiplication is 32-bits. */
    const uint64_t result1 = width * height;
    printf("Result 1: 0x%010lX\n", result1);

    /* However, if we cast one of its elements to something like `uint64_t', the
     * multiplication will evaluate to an `uint64_t'. */
    const uint64_t result2 = (uint64_t)width * height;
    printf("Result 2: 0x%010lX\n", result2);

    /* This is also INCORRECT. Even though the third element of the
     * multiplication evaluates to `size_t', the first multiplication is
     * "truncated" since it evaluates to `uint32_t'. */
    uint32_t* data0 = malloc(height * width * sizeof(uint32_t));
    free(data0);

    /* This is CORRECT, the first multiplication evaluates to `size_t'. */
    uint32_t* data1 = malloc(height * width * sizeof(uint32_t));
    free(data1);

    return 0;
}
