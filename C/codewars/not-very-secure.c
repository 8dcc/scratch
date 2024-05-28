/* Kata: https://www.codewars.com/kata/526dbd6c8c0eb53254000110 */

#include <stdbool.h>

bool alphanumeric(const char* str) {
    int c;

    // Iterate and check chars
    for (c = 0; str[c] != '\0'; c++) {
        int ch = str[c];
        if (ch < '0'
                || (ch > '9' && ch < 'A')
                || (ch > 'Z' && ch < 'a')
                || ch > 'z') return false;
    }

    // In the end return true only if c is not 0 (str was not empty)
    return (c != 0);
}
