/* Used for heap allocation in fs-os. Keep in mind we use uint64_t here but
 * uint32_t should be used in there. */

#include <stdint.h>
#include <stdio.h>

/* If the data pointer is not aligned to the macro's parameter, align it */
#define ALIGN(ptr, align)                                \
    do {                                                 \
        const size_t data_align = (uint64_t)ptr % align; \
        if (data_align != 0)                             \
            ptr = (uint8_t*)(ptr) + align - data_align;  \
    } while (0)

int main() {
    void* test_ptr = (void*)0xAB5; /* 2741 */
    printf("0x%lX, %ld\n", (uint64_t)test_ptr, (uint64_t)test_ptr);

    ALIGN(test_ptr, 8);
    printf("0x%lX, %ld  |  ", (uint64_t)test_ptr, (uint64_t)test_ptr);
    printf("%ld %%  8 -> %ld\n", (uint64_t)test_ptr, (uint64_t)test_ptr % 8);

    ALIGN(test_ptr, 32);
    printf("0x%lX, %ld  |  ", (uint64_t)test_ptr, (uint64_t)test_ptr);
    printf("%ld %% 32 -> %ld\n", (uint64_t)test_ptr, (uint64_t)test_ptr % 32);

    return 0;
}
