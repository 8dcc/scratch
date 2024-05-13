
#include <stdint.h>
#include <stdio.h>

/* Test for parsing C's "\NNN" octal escape sequences */
static uint8_t parseoct(const char* str) {
    uint8_t ret = 0;

    /* Conditional made on purpose to match other program's */
    if (*str >= '0' && *str <= '7') {
        for (;;) {
            /* Make sure we are not discarding bits, should
             * never happen in valid sequences. */
            if (ret & 0300)
                break;

            /* Max octal is 3 bits */
            ret <<= 3;
            ret |= *str - '0';

            /* Need to increase this way so we don't consume an
             * extra character. */
            if (str[1] >= '0' && str[1] <= '7')
                str++;
            else
                break;
        }
    }

    return ret;
}

/* Test for parsing C's "\xHH" hexadecimal escape sequences */
static uint8_t parsehex(const char* str) {
    uint8_t ret = 0;

    /* Conditional made on purpose to match other program's */
    if (*str == 'x') {
        for (;;) {
            /* Make sure we are not discarding bits, should never happen in
             * valid sequences. */
            if (ret & 0xF0)
                break;

            /* Unlike when parsing octal, we can increase the pointer here since
             * we have to consume the 'x' */
            str++;

            uint8_t digit = 0;
            if (*str >= '0' && *str <= '9')
                digit = *str - '0';
            else if (*str >= 'a' && *str <= 'f')
                digit = *str - 'a' + 0xA;
            else if (*str >= 'A' && *str <= 'F')
                digit = *str - 'A' + 0xA;
            else
                break;

            /* Max hex is 4 bits */
            ret <<= 4;
            ret |= digit;
        }
    }

    return ret;
}

int main(void) {
    uint8_t a = parseoct("123 ignored");
    printf("%u (o%o)\n", a, a);

    uint8_t b = parsehex("xA3 ignored");
    printf("%u (x%02X)\n", b, b);

    return 0;
}
