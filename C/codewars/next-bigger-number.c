/* Kata: https://www.codewars.com/kata/55983863da40caa2c900004e */
/* Works but too slow :( */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_LONG_CHARS 19

int digits_match(long long a, long long b, int digits);

long long next_bigger_number(long long num) {
    // Iterate each digit
    int digits = 0;
    for (long long n = num; n > 0; n /= 10)
        digits++;

    long long ret = num;
    for (long long n = num+1; n < (long long)pow(10, digits) - 1; n++) {
        if (digits_match(num, n, digits)) {
            ret = n;
            break;
        }
    }

    return (ret == num) ? -1 : ret;
}

// Checks if all digits of a are in b
int digits_match(long long a, long long b, int digits) {
    long long* a_digits = malloc(digits * sizeof(long long));
    memset(a_digits, -1, sizeof(long long));

    // Iterate each digit. n will be 1 digit less each iteration
    int i = 0;
    for (long long n = a; n > 0; n /= 10)
        a_digits[i++] = n % 10;

    // Iterate b and check if any digit does not match
    for (long long n = b; n > 0; n /= 10) {
        int cur_digit = n % 10;

        // Iterate a_digits[]
        int contained = 0;
        for (long long i = 0; i < digits; i++) {
            if (cur_digit == a_digits[i]) {
                contained = 1;
                a_digits[i] = -1;
                break;
            }
        }

        if (contained == 0) return 0;       // Didn't match
    }

    return 1;       // Matched
}
