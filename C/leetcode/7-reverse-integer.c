
#include <stdio.h>
#include <limits.h>

/*
 * https://leetcode.com/problems/reverse-integer/
 *
 * 7. Reverse Integer
 * ==================
 *
 * Given a signed 32-bit integer `x', return `x' with its digits reversed. If
 * reversing `x' causes the value to go outside the signed 32-bit integer range
 * `[-231, 231 - 1]', then return 0.
 *
 * Assume the environment does not allow you to store 64-bit integers (signed or
 * unsigned).
 *
 * Examples
 * --------
 *
 * Input: x = 123
 * Output: 321
 *
 * Input: x = -123
 * Output: -321
 *
 * Input: x = 120
 * Output: 21
 *
 * Notes
 * -----
 *
 * I thought it was going to be harder because of signedness, but the only thing
 * that had to change was the overflow conditional, since we can hold an extra
 * negative value.
 */

int reverse(int x) {
    int result = 0;

    while (x != 0) {
        const int digit = x % 10;

        /*
         * INT_MIN: -2147483648
         * INT_MAX:  2147483647
         *
         * Since we have to multiply by 10, if the result before "appending" the
         * digit is less than -214748364 or greater than 214748364, we know it
         * will overflow. However, if it's equal to those values, we have to
         * check if the digit we are "appending" will cause the number to
         * overflow. For negative values we have to make sure the digit is not
         * smaller than -8, and for positive values we have to make sure it's
         * not greater than 7.
         *
         * We assume that the compiler will optimize the constant divisions, but
         * we could calculate them once outside the loop instead of dividing
         * each iteration.
         */
        if (result < INT_MIN / 10 || (result == INT_MIN / 10 && digit < -8) ||
            result > INT_MAX / 10 || (result == INT_MAX / 10 && digit > 7))
            return 0;

        result *= 10;
        result += digit;
        x /= 10;
    }

    return result;
}

int main(void) {
    printf("Result 1: %d\n", reverse(123));
    printf("Result 2: %d\n", reverse(-123));
    printf("Result 3: %d\n", reverse(120));

    printf("Result 4: %d\n", reverse(1000000002));
    printf("Result 5: %d\n", reverse(1000000003));
    printf("Result 6: %d\n", reverse(-1000000002));
    printf("Result 7: %d\n", reverse(-1000000003));

    printf("Result 8: %d\n", reverse(INT_MIN));
    printf("Result 9: %d\n", reverse(INT_MAX));
    return 0;
}
