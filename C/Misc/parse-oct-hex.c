
#include <stdint.h>
#include <stdio.h>

/* Test for parsing C's "\NNN" octal escape sequences */
static uint8_t parseoct(const char* str) {
    uint8_t ret = 0;

    while (*str >= '0' && *str <= '7') {
        /* Make sure we are not discarding bits, should never
         * happen in valid sequences. */
        if (ret & 0700)
            break;

        /* Max octal is 3 bits */
        ret <<= 3;
        ret |= *str - '0';

        str++;
    }

    return ret;
}

/* Test for parsing C's "\xHH" hexadecimal escape sequences */
static uint8_t parsehex(const char* str) {
    uint8_t ret = 0;

    /* Conditional made on purpose to match other program's */
    if (*str == 'x') {
        str++;

        for (;;) {
            /* Make sure we are not discarding bits, should
             * never happen in valid sequences. */
            if (ret & 0xF0)
                break;

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

            str++;
        }
    }

    return ret;
}

int main(void) {
    printf("%u\n", parseoct("123 ignored"));
    printf("%u\n", parsehex("xA3 ignored"));
    return 0;
}
