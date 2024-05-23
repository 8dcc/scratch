/* 4-02 */

/*
 * Compile using "-lm" for linking the math lib:
 *   gcc -o atof-extended.out 4-02-atof-extended.c -lm
 */

#include <stdio.h>
#include <math.h>     // pow()
#include <ctype.h>    // isdigit()

#define DIG2INT(c) (c - '0')

static int atoi(const char* s);
static double atof(const char* s);

int main() {
    const char* s1 = "420.1337";
    const char* s2 = "-1.5^4";
    const char* s3 = "12.5e-4";
    const char* s4 = "-12.5e-4";
    const char* s5 = "1.56e4";

    printf("\"%s\" => %f\n", s1, atof(s1));
    printf("\"%s\" => %f\n", s2, atof(s2));
    printf("\"%s\" => %f\n", s3, atof(s3));
    printf("\"%s\" => %f\n", s4, atof(s4));
    printf("\"%s\" => %f\n", s5, atof(s5));

    return 0;
}

/*
 * Converts the string s into an double. Supports signed integer exponents using:
 *   12.56^-10 => pow(12.56, -10)
 *
 * And cientific notation (also signed) using:
 *   12.56e5 => 12.56 * 10 ^ 5 => 1256000
 */
double atof(const char* s) {
    double ret = 0.0;
    double dp  = 1.0;    // Decimal power. Used to store how many decimals we have.
                         // "123.789" => 123789 (dp = 10^3) => 123.789
    int i   = 0;
    int exp = 0;    // Exponent: "...e-10"

    // Get sign multiplier
    int sign = (s[i] == '-') ? -1 : 1;
    if (s[i] == '-' || s[i] == '+') i++;

    // Start of str after sign to '.'
    while (isdigit(s[i]))
        ret = ret * 10 + DIG2INT(s[i++]);    // Increase i in the same place

    // Skip the dot. We will get decimals in the next for
    if (s[i] == '.') i++;

    // Same as the other part, but we increase the dp variable in the for loop
    for (dp = 1.0; isdigit(s[i]); dp *= 10.0)
        ret = ret * 10 + DIG2INT(s[i++]);    // Increase i in place as well

    // See dp comment to understand the division
    ret = sign * (ret / dp);

    // Exponent
    char exp_c = s[i];
    if (exp_c == '^' || exp_c == 'e') {
        i++;

        // Sign of exponent
        int exp_sign = (s[i] == '-') ? -1 : 1;
        if (s[i] == '-' || s[i] == '+') i++;

        // Each digit of exponent
        while (isdigit(s[i]))
            exp = exp * 10 + DIG2INT(s[i++]);    // Increase i in place as well

        exp *= exp_sign;

        if (exp_c == '^')
            ret = pow(ret, exp);
        else    // 'e'
            ret *= pow(10, exp);
    }

    return ret;
}

/* Converts the string s into an integer. Used for the exponent of atof. */
int atoi(const char* s) {
    int ret = 0;

    return ret;
}
