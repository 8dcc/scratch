#include <stdio.h>

/* From memcpy-asm.asm */
extern void* memcpy_asm(void* dst, const void* src, size_t n);

void* memcpy_c(void* dst, const void* src, size_t n) {
    size_t i;
    for (i = 0; i < n; i++)
        *(char*)dst++ = *(char*)src++;
    return dst;
}

int main(void) {
    const char* src = "TESTING";
    char dst[8];

    //memcpy_c(dst, src, sizeof(src));
    memcpy_asm(dst, src, sizeof(src));

    printf("%s\n", dst);

    return 0;
}
