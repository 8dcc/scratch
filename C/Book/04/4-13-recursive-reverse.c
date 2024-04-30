/* 4-13 */

#include <stdio.h>
#include <string.h>

char* rreverse(char* str);

int main() {
    // We declare them as arrays and not as char* because that would point to a const
    // literal, and rreverse would fail.
    char str1[] = "Hello, world!";    // 13
    char str2[] = "Hello, world";     // 12

    printf("\"%s\" ->", str1);
    printf("\"%s\"\n", rreverse(str1));
    printf("\"%s\" ->", str2);
    printf("\"%s\"\n", rreverse(str2));

    return 0;
}

// Reverses str in place, returns str.
char* rreverse(char* str) {
    static int sp = 0;
    int sl        = 0;
    int cbuf      = 0;

    // 0 to middle of str
    if (sp < (sl = strlen(str)) / 2 && str[sp] != '\0') {
        // Swap
        cbuf             = str[sp];
        str[sp]          = str[sl - 1 - sp];
        str[sl - 1 - sp] = cbuf;

        // Increase str pos and call rreverse again
        sp++;
        rreverse(str);
    }

    if (sp > 0) sp--;

    return str;
}
