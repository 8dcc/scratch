#ifndef UTIL_H_
#define UTIL_H_

#include <stdarg.h>
#include <stdio.h>

#define ERR(...) err_msg(__func__, __VA_ARGS__)

static inline void err_msg(const char* func, const char* fmt, ...) {
    va_list va;
    va_start(va, fmt);

    fprintf(stderr, "%s: ", func);
    vfprintf(stderr, fmt, va);
    fprintf(stderr, "\n");

    va_end(va);
}

#endif    // UTIL_H_
