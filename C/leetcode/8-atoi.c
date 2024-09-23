
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>  /* isspace(), isdigit() */
#include <limits.h> /* INT_MIN, INT_MAX */

/*
 * https://leetcode.com/problems/string-to-integer-atoi/
 *
 * 8. String to Integer (atoi)
 * ===========================
 *
 * Implement the `myAtoi(string s)' function, which converts a string to a
 * 32-bit signed integer.
 *
 * The algorithm for `myAtoi(string s)' is as follows:
 *
 *  1. Whitespace: Ignore any leading whitespace (" ").
 *  2. Signedness: Determine the sign by checking if the next character is '-'
 *     or '+', assuming positivity is neither present.
 *  3. Conversion: Read the integer by skipping leading zeros until a non-digit
 *     character is encountered or the end of the string is reached. If no
 *     digits were read, then the result is 0.
 *  4. Rounding: If the integer is out of the 32-bit signed integer range
 *     `[-2^31, 2^31 - 1]', then round the integer to remain in the
 *     range. Specifically, integers less than `-2^31' should be rounded to
 *     `-2^31', and integers greater than `2^31 - 1' should be rounded to
 *     `2^31 - 1'.
 *
 * Return the integer as the final result.
 *
 * Examples
 * --------
 *
 * - Input: s = "42"
 * - Output: 42
 * - Explanation: The underlined characters are what is read in and the caret is
 *   the current reader position.
 *
 *     Step 1: "42"
 *              ^ (no characters read because there is no leading whitespace)
 *     Step 2: "42"
 *              ^ (no characters read because there is neither a '-' nor '+')
 *     Step 3: "42"
 *                ^ ("42" is read in)
 *
 * - Input: s = " -042"
 * - Output: -42
 * - Explanation:
 *
 *     Step 1: "   -042"
 *                 ^ (leading whitespace is read and ignored)
 *     Step 2: "   -042"
 *                  ^ ('-' is read, so the result should be negative)
 *     Step 3: "   -042"
 *                    ^ ("042" is read in, leading zeros ignored in the result)
 *
 * - Input: s = "1337c0d3"
 * - Output: 1337
 * - Explanation:
 *
 *     Step 1: "1337c0d3"
 *              ^ (no characters read because there is no leading whitespace)
 *     Step 2: "1337c0d3"
 *              ^ (no characters read because there is neither a '-' nor '+')
 *     Step 3: "1337c0d3"
 *                  ^ ("1337" is read in; reading stops because the next
 *                     character is a non-digit)
 *
 * - Input: s = "0-1"
 * - Output: 0
 * - Explanation:
 *
 *     Step 1: "0-1"
 *              ^ (no characters read because there is no leading whitespace)
 *     Step 2: "0-1"
 *              ^ (no characters read because there is neither a '-' nor '+')
 *     Step 3: "0-1"
 *               ^ ("0" is read in; reading stops because the next character is
 *                  a non-digit)
 *
 * - Input: s = "words and 987"
 * - Output: 0
 * - Explanation: Reading stops at the first non-digit character 'w'.
 *
 * Notes
 * -----
 *
 * The regular expression (PCRE) for a valid input string is:
 *   "^\s*[-+]?0*\d+\D*$"
 */

int myAtoi(char* s) {
    /* Skip leading spaces */
    while (isspace(*s))
        s++;

    /* Check the sign */
    const bool negative = (*s == '-');
    if (*s == '-' || *s == '+')
        s++;

    /* Skip leading zeros */
    while (*s == '0')
        s++;

    /*
     * The `result' variable is unsigned, but it's going to be converted to a
     * signed integer before returning. Get the maximum unsigned value it can
     * store before detecting an underflow/overflow.
     *
     * If the result is positive, the maximum unsigned value we can store is
     * `INT_MAX' (2147483647).
     *
     * If the result is negative, the maximum unsigned value we can store is
     * `-(INT_MIN)', or `INT_MAX+1' (2147483648).
     *
     * Therefore, if the value before the multiplication is 214748364, we only
     * have to check whether the digit we are appending is 7 or 8. See leetcode
     * #7 for more information.
     *
     * (As a side note, I chose the `init' and `last' names because of Haskell.)
     */
    const unsigned int max = (negative) ? (unsigned int)INT_MAX + 1 : INT_MAX;
    const unsigned int max_init = max / 10;
    const unsigned int max_last = max % 10;

    /* Read the actual number */
    unsigned int result = 0;
    while (isdigit(*s)) {
        const unsigned int digit = *s - '0';

        if (result > max_init || (result == max_init && digit > max_last)) {
            result = max;
            break;
        }

        result *= 10;
        result += digit;
        s++;
    }

    return (negative) ? -result : result;
}

int main(void) {
    char case1[] = "42";
    printf("'%s' -> %d\n", case1, myAtoi(case1));

    char case2[] = "   -042";
    printf("'%s' -> %d\n", case2, myAtoi(case2));

    char case3[] = "1337c0d3";
    printf("'%s' -> %d\n", case3, myAtoi(case3));

    char case4[] = "0-1";
    printf("'%s' -> %d\n", case4, myAtoi(case4));

    char case5[] = "words and 987";
    printf("'%s' -> %d\n", case5, myAtoi(case5));

    return 0;
}
