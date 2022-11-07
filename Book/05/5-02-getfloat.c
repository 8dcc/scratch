/* 5-01 */
/*
 * You can see the differences between the 5-01 and 5-02 with:
 *   diff -u --color=auto 5-01* 5-02*
 */

#include <stdio.h>
#include <ctype.h>

int dpow(int b, int e);
int getfloat(float* num);

int main() {
    float num = 0;
    int c     = 0;

    // Each call will parse a char from stdin. Will return the char it got, and
    // append the number in case it's a digit
    while ((c = getfloat(&num)) != EOF && c != '\n')
        // Not a digit, because we keep getting digits on getint
        printf("Got unknown char: %c\n", c);

    printf("Final number: %f\n", num);

    return 0;
}

/*
 * getfloat: reads characters from stdin and saves floats into num. Returns non-digit
 * chars.
 * We need to return int (chars), no need for floats.
 */
int getfloat(float* num) {
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

    // If there is a decimal part
    if (c == '.') {
        int dec_pow = 0;
        for (dec_pow = 0; isdigit(c = getchar()); dec_pow++)
            *num = *num * 10 + (c - '0');

        *num /= dpow(10, dec_pow);    // Move decimal pos
    }

    *num *= sign;

    // After possible spaces, possible sign, and all digits, return chars
    return c;
}

/* dpow: simple decimal power function so I don't have to link math.h */
int dpow(int b, int e) {
    int ret = 1;

    for (int i = 0; i < e; i++) ret *= b;

    return ret;
}
