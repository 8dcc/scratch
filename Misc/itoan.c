/* Used to test stdlib stuff for fs-os */

#include <stdio.h>

/* count_digits: returns the number of digits of a positive num. Will not count "-"
 * for negative numbers */
int count_digits(int num) {
    int ret = 1;

    /* Count how many numbers we can remove */
    while ((num /= 10) > 0) {
        ret++;
    }

    return ret;
}

/* ipow: integer power. Returns b^e */
int ipow(int b, int e) {
    int ret = 1;

    /* Multiply by the base "e" times */
    while (e-- > 0)
        ret *= b;

    return ret;
}

/* itoan: write the first "max_digits" of "num" (at max) into "str". "str" needs to
 * have enough space. Useful for making sure you won't write out of bounds. Keep in
 * mind that max_digits does not include the null terminator. */
void itoan(char* str, int num, size_t max_digits) {
    if (max_digits <= 0) {
        str[0] = '\0';
        return;
    }

    /* sp is the current position we want to write in the string */
    size_t sp = 0;

    /* Write '-' for negative numbers and convert number to positive */
    if (num < 0) {
        str[sp++] = '-';
        num       = -num;
    }

    /*
     * cur_digit will count the current number we want to write, starting from the
     * right, where the last one is 0. So for 354, cur_digit will be 2, and 354 / 100
     * is 3. If cur_digit is 1, 354 / 10 = 35, and we can extract the last digit by
     * doing (n%10)
     *
     * Also make sure the string position is not greater than the max we can write.
     */
    int cur_digit;
    for (cur_digit = count_digits(num) - 1; cur_digit > 0 && sp < max_digits;
         cur_digit--)
        str[sp++] = (num / ipow(10, cur_digit)) % 10 + '0';

    /* Last digit */
    if (cur_digit == 0)
        str[sp++] = num % 10 + '0';

    str[sp++] = '\0';
}

#define TEST(num, limit)                                             \
    {                                                                \
        itoan(arr, num, limit);                                      \
        printf("num: %6d, limit: %3d -> \"%s\"\n", num, limit, arr); \
    }

int main() {
    char arr[255] = { 0 };

    TEST(123, 254);
    TEST(123, 3);
    TEST(1234, 3);
    TEST(0, 3);
    TEST(1234, 0);
    TEST(-1234, 4);
    TEST(-1234, 5);

    return 0;
}
