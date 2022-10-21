/* 3-06 */

#include <stdio.h>
#include <string.h>    // strlen()
#include <stdlib.h>    // calloc()

static char* reverse(char* s);
static char* itoa(int n, char* s, int min_c);

int main() {
    char str[255] = { 0 };

    memset(str, '\0', sizeof(str));
    itoa(1337, str, 10);
    printf("\"%s\"\n", str);

    memset(str, '\0', sizeof(str));
    itoa(-1337, str, 5);
    printf("\"%s\"\n", str);

    return 0;
}

/*
 * Converts n into a string, fills the characters needed to reach min_c, and writes
 * it to s. Returns s.
 */
char* itoa(int n, char* s, int min_c) {
    int sp   = 0;    // Current char pos in s
    int sign = 0;    // 0 means positive

    if (n < 0) {
        sign = 1;
        n    = -n;
    }

    // We use do while because we would also need to run 0
    do {
        s[sp++] = (n % 10) + '0';    // Last digit + ascii value of 0
        n /= 10;                     // Remove last char
    } while (n > 0);

    // After getting the reversed string with the digits, add the sign.
    if (sign) s[sp++] = '-';

    // Add spaces to compensate min_c
    while (sp - 1 < min_c) s[sp++] = ' ';

    // Same as 3-05. Check comment there.
    reverse(s);
    return s;
}

/* Reverse string in place using a buffer */
char* reverse(char* s) {
    const int s_len = strlen(s);
    char* buff      = calloc(s_len, sizeof(char));
    int buff_p      = 0;

    for (int i = s_len - 1; i >= 0; i--) buff[buff_p++] = s[i];

    strcpy(s, buff);

    free(buff);
    return s;
}
