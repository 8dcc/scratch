/* 4-12 */

#include <stdio.h>
#include <math.h>

int ritoa(char* str, int num);

/*
 * These were static but it was too messy after calling functions, so I made them
 * globals so I can reset them after calling the functions.
 */
int sp = 0;    // str pos
int nd = 0;    // num digits. Needed so we don't use +1 as exponent when num is
               // negative ('-')

int main() {
    char s1[100] = { 0 };
    char s2[100] = { 0 };
    char s3[100] = { 0 };

    ritoa(s1, 420);     sp = 0; nd = 0;
    ritoa(s2, -1337);   sp = 0; nd = 0;
    ritoa(s3, 1337);    sp = 0; nd = 0;    

    printf("s1 (420)   -> \"%s\"\n"
           "s2 (-1337) -> \"%s\"\n"
           "s3 (1337)  -> \"%s\"\n",
           s1, s2, s3);

    return 0;
}

/*
 * Example of recursion. Number next to "" is supposed to be sp, or the idx of the
 * str we are accessing.

ritoa(""(0), 1234)
    check_sign

    ritoa(""(1), 1234)
        ritoa(""(2), 1234)
            ritoa(""(3), 1234)
                1234/10000 is not more than 0
                    "    \0"(3 + 1)

                "   4\0"(3 -> 2)
                return 123
            "   34\0"(2 -> 1)
            return 12
        " 234\0"(1->0)
        return 1
    "1234\0"(0)

 */
int ritoa(char* str, int num) {
    if (num < 0) {
        str[sp++] = '-';
        num       = -num;
    }

    int ret = 0;

    // Check if:
    //   123 / 10 > 0       (sp = 0)
    //   123 / 100 > 0      (sp = 1)
    //   123 / 1000 > 0     (sp = 2)
    if (num / (int)pow(10, nd + 1) > 0) {
        sp++;
        nd++;
        ret = ritoa(str, num);
    } else {
        str[sp + 1] = '\0';
        ret = num;    // If we are in the last recursion, the digit we care about is
                      // the last one of the original one. See the top comment.
    }

    // Append last char to str. Because we are calling ritoa first, we will start
    // appending to str when sp is at the position of the last char, to 0 or 1 ('-')
    str[sp--] = ret % 10 + '0';
    nd--;    // TODO: Negatives

    // After we are done with the number, check again if it's negative to subract sp
    // for the next function call. This negative condition will only be *possible* on
    // the first call of ritoa(), because we are doing n = -n on the first if
    if (num < 0) sp--;

    return ret / 10;
}
