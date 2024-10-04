
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

bool concat_format(char** dst, size_t* dst_sz, size_t* dst_offset,
                   const char* fmt, ...) {
    va_list va;

    va_start(va, fmt);
    const int data_size = vsnprintf(NULL, 0, fmt, va);
    va_end(va);

    if (data_size < 0) {
        fprintf(stderr, "Failed to get the target string length for format: %s",
                fmt);
        return false;
    }

    if (*dst_offset + data_size + 1 >= *dst_sz) {
        *dst_sz = *dst_offset + data_size + 1;
        *dst    = realloc(*dst, *dst_sz);
    }

    char* real_dst    = &(*dst)[*dst_offset];

    va_start(va, fmt);
    const int written = vsnprintf(real_dst, data_size + 1, fmt, va);
    va_end(va);

    *dst_offset += written;
    return true;
}

int main(void) {
    size_t str_pos = 0;
    size_t str_sz  = 15;
    char* str      = malloc(str_sz);

    concat_format(&str, &str_sz, &str_pos, "%s", "Testing...");
    concat_format(&str, &str_sz, &str_pos, " %lld ", 1337LL);
    concat_format(&str, &str_sz, &str_pos, "%#x", 0xDEADBEEF);

    printf("Result: \"%s\"\n", str);

    free(str);
    return 0;
}
