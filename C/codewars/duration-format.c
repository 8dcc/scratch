/* https://www.codewars.com/kata/52742f58faf5485cae000b9a */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SUM_STRS(data, data_str)                               \
    {                                                          \
        if (data) {                                            \
            if (printed) {                                     \
                if (print_and) {                               \
                    sprintf(buf2, "%s and %s", data_str, buf); \
                    print_and = false;                         \
                } else {                                       \
                    sprintf(buf2, "%s, %s", data_str, buf);    \
                }                                              \
            } else {                                           \
                sprintf(buf2, "%s", data_str);                 \
                printed = true;                                \
            }                                                  \
        }                                                      \
        sprintf(buf, "%s", buf2); /* Copy to main buf */       \
    }

char* formatDuration(int n) {
    char* buf = calloc(255, sizeof(char));

    if (n == 0) {
        sprintf(buf, "now");
        return buf;
    }

    const uint32_t y = (n / 31536000);
    const uint32_t d = (n - y * 31536000) / 86400;
    const uint32_t h = (n - y * 31536000 - d * 86400) / 3600;
    const uint32_t m = (n - y * 31536000 - d * 86400 - h * 3600) / 60;
    const uint32_t s = (n - y * 31536000 - d * 86400 - h * 3600 - m * 60);

    char* ys = calloc(100, sizeof(char));
    char* ds = calloc(100, sizeof(char));
    char* hs = calloc(100, sizeof(char));
    char* ms = calloc(100, sizeof(char));
    char* ss = calloc(100, sizeof(char));

    if (y == 1)
        sprintf(ys, "%d year", y);
    else
        sprintf(ys, "%d years", y);

    if (d == 1)
        sprintf(ds, "%d day", d);
    else
        sprintf(ds, "%d days", d);

    if (h == 1)
        sprintf(hs, "%d hour", h);
    else
        sprintf(hs, "%d hours", h);

    if (m == 1)
        sprintf(ms, "%d minute", m);
    else
        sprintf(ms, "%d minutes", m);

    if (s == 1)
        sprintf(ss, "%d second", s);
    else
        sprintf(ss, "%d seconds", s);

    bool print_and = true;
    bool printed = false;
    char* buf2 = calloc(255, sizeof(char));

    if (s) {
        sprintf(buf2, "%s", ss);
        printed = true;
    }
    sprintf(buf, "%s", buf2); /* Copy to main buf */

    SUM_STRS(m, ms);
    SUM_STRS(h, hs);
    SUM_STRS(d, ds);
    SUM_STRS(y, ys);

    /* sprintf(buf, "%s%s%s%s%s", ys, ds, ms, hs, ss); */

    free(ys);
    free(ds);
    free(hs);
    free(ms);
    free(ss);
    free(buf2);

    return buf;
}

