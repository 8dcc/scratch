/* Kata: https://www.codewars.com/kata/55911ef14065454c75000062 */

#include <stdio.h>    // delme
#include <stdlib.h>
#include <string.h>

#define swap(a, b)                \
    {                             \
        const char* tmp = a;      \
        a               = b;      \
        b               = tmp;    \
        int tmp_sz      = a_sz;   \
        a_sz            = b_sz;   \
        b_sz            = tmp_sz; \
    }

/* Str8 up from fs-os */
char* strrev(char* str) {
    const int len = strlen(str);
    char c        = 0;

    for (int i = 0; i < len / 2 && str[i] != '\0'; i++) {
        c = str[i];

        /* -1 to convert length to idx */
        str[i]           = str[len - 1 - i];
        str[len - 1 - i] = c;
    }

    return str;
}

char* multiply(const char* a, const char* b) {
    int a_sz = strlen(a) - 1;
    int b_sz = strlen(b) - 1;

    /* Bigger one always in a */
    if (b_sz > a_sz)
        swap(a, b);

    char* ret  = calloc(a_sz + b_sz + 3, sizeof(char));
    int retpos = 0; /* Start at 0, store in reverse order */

    /* Iterate from right char to left of the biggest string */
    int rem = 0;
    for (int apos = a_sz; apos >= 0; apos--) {
        /* Starting position for next multiplication */
        retpos = a_sz - apos;

        for (int bpos = b_sz; bpos >= 0; bpos--) {
            const int a_digit = a[apos] - '0';
            const int b_digit = (bpos >= 0) ? b[bpos--] - '0' : 0;
            const int final   = a_digit * b_digit + rem;

            /* Lost my mind */
            ret[retpos] += final % 10;
            int tmppos = retpos;
            while (ret[tmppos] > 0) {
                ret[tmppos + 1] += ret[tmppos] % 10;
                ret[tmppos] = ret[tmppos] / 10;
                tmppos++;
            }

            retpos++;
            rem = final / 10;
        }

        /* Last rem */
        while (rem > 0) {
            ret[retpos++] += rem % 10;
            rem /= 10;
        }
    }

    /* Convert to str */
    for (int i = 0; i <= retpos; i++) {
        ret[i] += '0';
    }

    strrev(ret);
    return ret;
}
