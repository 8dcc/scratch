/* 5-01 */

#include <stdio.h>
#include <ctype.h>

int getint(int* num);

int main() {
    int num = 0;
    int c   = 0;

    // Each call will parse a char from stdin. Will return the char it got, and
    // append the number in case it's a digit
    while ((c = getint(&num)) != EOF && c != '\n')
        // Not a digit, because we keep getting digits on getint
        printf("Got unknown char: %c\n", c);

    printf("Final number: %d\n", num);

    return 0;
}

/*
 * getint: reads characters from stdin and saves numbers into num. Returns non-digit
 * chars.
 * I was not really sure what the exercise asked so I did my own getint.
 */
int getint(int* num) {
    int c = 0;

    while (isspace(c = getchar()))
        ;

    int sign = (c == '-') ? -1 : 1;
    if (c == '-' || c == '+') c = getchar();

    *num = 0;
    while (isdigit(c)) {
        // Add one digit, convert char to int and append it
        *num = *num * 10 + (c - '0');
        c    = getchar();
    }

    *num *= sign;

    // After possible spaces, possible sign, and all digits, return chars
    return c;
}
