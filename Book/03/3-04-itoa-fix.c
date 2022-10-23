/* 3-04 */

/*
 * The minimum signed int number is 1 more than the bigger, so when doing
 * n = -n
 * We will not reach the minimum number. To solve this, we dont convert to positive
 * when getting the sign, but we convert to positive each digit. I made a function
 * for that but you can use abs() from stdlib.h
 *
 * I also made an alternate version of reverse.
 */

#include <stdio.h>
#include <limits.h>    // INT_MAX, INT_MIN
#include <string.h>    // strlen()
#include <stdlib.h>    // calloc()

static char* reverse(char* s);
static char* reverse2(char* s);    // Not used
static inline int absolute(int n);
static char* itoa(int n, char* s);

int main() {
    char str[255] = { 0 };

    memset(str, '\0', sizeof(str));
    itoa(0, str);
    printf("\"%s\"\n", str);

    memset(str, '\0', sizeof(str));
    itoa(INT_MAX, str);
    printf("\"%s\"\n", str);

    memset(str, '\0', sizeof(str));
    itoa(INT_MIN, str);
    printf("\"%s\"\n", str);

    printf("\nTesting reverse2:\n");

    char s1[] = "abcd";
    printf("\"%s\" => ", s1);
    reverse2(s1);
    printf("\"%s\"\n", s1);

    char s2[] = "12345";
    printf("\"%s\" => ", s2);
    reverse2(s2);
    printf("\"%s\"\n", s2);

    return 0;
}

/* Converts n into a string and writes it to s. Returns s. */
char* itoa(int n, char* s) {
    int sp   = 0;    // Current char pos in s
    int sign = 0;    // 0 means positive

    if (n < 0) sign = 1;

    // We use do while because we would also need to run 0
    do {
        s[sp++] = absolute(n % 10) + '0';    // Last digit + ascii value of 0
        n /= 10;                             // Remove last char
    } while (n != 0);

    // After getting the reversed string with the digits, add the sign.
    if (sign) s[sp++] = '-';
    
    s[sp] = '\0';

    // More info and comments in 3-05
    reverse(s);
    return s;
}

int absolute(int n) {
    return (n < 0) ? -n : n;
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

/* Alternate version. Reverse string in place using a char buffer */
char* reverse2(char* s) {
    const int s_len = strlen(s);
    char buff       = 0;

    /*
     * Go from s[0] to s[s_len/2] and replace each char with the char from the other
     * half:
     * s = [ H E L L O ] => [ O L L E H ]
     *       | |   | |
     *       | ----- |
     *       |-------|
     */
    for (int i = 0; i < s_len / 2 && s[i] != '\0'; i++) {
        buff = s[i];

        // -1 Because s_len is the length but we need to access the idx (starts at 0)
        s[i]             = s[s_len - 1 - i];
        s[s_len - 1 - i] = buff;
    }

    return s;
}
