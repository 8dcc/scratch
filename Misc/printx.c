
#include <stdio.h>
#include <string.h>
#include <limits.h>

/* strrev: reverse str in place without allocating. Return str */
char* strrev(char* str) {
    const int len = strlen(str);
    char c        = 0;

    /*
     * Go from str[0] to str[len/2] and replace each char with the char from the
     * other half:
     * s = [ H E L L O ] => [ O L L E H ]
     *       | |   | |
     *       | ----- |
     *       |-------|
     */
    for (int i = 0; i < len / 2 && str[i] != '\0'; i++) {
        c = str[i];

        /* -1 to convert length to idx */
        str[i]           = str[len - 1 - i];
        str[len - 1 - i] = c;
    }

    return str;
}

/* print: calls "putchar" for each char of "str". Returns bytes written. */
static inline int print(const char* str) {
    while (*str != '\0')
        putchar(*str++);

    return 0;
}

/* printx: print "num" in hexadecimal format (lowercase) */
static void printx(unsigned long num) {
    /* max digits of an unsigned long */
    char hex_str[12] = { 0 };

    int tmp = 0;
    size_t i;
    for (i = 0; num > 0 && i < sizeof(hex_str) - 1; i++) {
        tmp = num % 16;
        num /= 16;

        /* Convert to char */
        tmp += (tmp < 10) ? '0' : 'a' - 10;

        hex_str[i] = tmp;
    }

    hex_str[i] = '\0';

    /* Reverse string and print */
    strrev(hex_str);
    print(hex_str);
}

/* printX: print "num" in hexadecimal format (uppercase) */
static void printX(unsigned long num) {
    /* max digits of an unsigned long */
    char hex_str[12] = { 0 };

    int tmp = 0;
    size_t i;
    for (i = 0; num > 0 && i < sizeof(hex_str) - 1; i++) {
        tmp = num % 16;
        num /= 16;

        /* Convert to char */
        tmp += (tmp < 10) ? '0' : 'A' - 10;

        hex_str[i] = tmp;
    }

    hex_str[i] = '\0';

    /* Reverse string and print */
    strrev(hex_str);
    print(hex_str);
}

int main() {
    int test = 12345;

    printx(test);
    putchar('\n');

    test += 50;

    printx(test);
    putchar('\n');

    printX(test);
    putchar('\n');

    printx(ULONG_MAX);
    putchar('\n');

    return 0;
}
