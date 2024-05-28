/* Kata: https://www.codewars.com/kata/5727bb0fe81185ae62000ae3 */

#include <stdlib.h>

char* strclr(const char* s) {
    char* original = calloc(1000, sizeof(char));
    char* ret      = original;

    while (*s != '\0') {
        if (*s == '#') {
            if (ret > original)
                ret--;
        } else {
            *ret++ = *s;
        }

        s++;
    }

    *ret = '\0';
    return original;
}

