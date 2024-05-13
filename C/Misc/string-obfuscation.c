/*
 * XOR strings in a C source file with random key.
 * Usage: ./string-obfuscation.out < input.c > output.c
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h> /* srand, rand */
#include <string.h> /* strlen */
#include <time.h>   /* time */

// TODO
#define COMMENT_START "/* XOR(\""
#define COMMENT_END   "\") */\n"
#define ARR1_STR      "char s1[] = \""
#define ARR2_STR      "char s2[] = \""
#define ARR_END       "\";\n"

/* Convert a C string into the real array of bytes that would be compiled. The
 * allocated array must be freed by the caller.
 * The length of the new array will be written to `length' */
static uint8_t* str2bytes(const char* str, size_t* length) {
    size_t i = 0, ret_sz = 100;
    uint8_t* ret = calloc(ret_sz, sizeof(uint8_t));

    bool done = false;
    while (!done) {
        /* Reallocate output array if we ran out of space */
        if (i >= ret_sz) {
            ret_sz += 100;
            ret = realloc(ret, ret_sz);
        }

        /* Process the current character of the input */
        switch (*str) {
            case '\\':
                /* Always asume we are escaping. See:
                 * https://en.wikipedia.org/wiki/Escape_sequences_in_C */
                str++;
                switch (*str) {
                    case '\"':
                        /* If we encountered a closing quote, and it was
                         * escaped, add it literally. */
                        ret[i++] = '\"';
                        break;
                    case '\\':
                        /* Only insert backslash if it was escaped */
                        ret[i++] = '\\';
                        break;
                    case 'a':
                        /* Possible character escape sequences.
                         * \a \b \e \f \n \r \t \v */
                        ret[i++] = '\a';
                        break;
                    case 'b':
                        ret[i++] = '\b';
                        break;
                    case 'e':
                        ret[i++] = '\e';
                        break;
                    case 'f':
                        ret[i++] = '\f';
                        break;
                    case 'n':
                        ret[i++] = '\n';
                        break;
                    case 'r':
                        ret[i++] = '\r';
                        break;
                    case 't':
                        ret[i++] = '\t';
                        break;
                    case 'v':
                        ret[i++] = '\v';
                        break;
                    case 'x':
                        /* Search for "\xHH" sequences */
                        ret[i] = 0;

                        for (;;) {
                            /* Make sure we are not discarding bits, should
                             * never happen in valid sequences. */
                            if (ret[i] & 0xF0)
                                break;

                            /* Unlike when parsing octal, we can increase the
                             * pointer here since we have to consume the 'x' */
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
                            ret[i] <<= 4;
                            ret[i] |= digit;
                        }

                        i++;
                        break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                        /* Search for "\NNN" sequences. */
                        ret[i] = 0;

                        for (;;) {
                            /* Make sure we are not discarding bits, should
                             * never happen in valid sequences. */
                            if (ret[i] & 0300)
                                break;

                            /* Max octal is 3 bits */
                            ret[i] <<= 3;
                            ret[i] |= *str - '0';

                            /* Need to increase this way so we don't consume an
                             * extra character. */
                            if (str[1] >= '0' && str[1] <= '7')
                                str++;
                            else
                                break;
                        }

                        i++;
                        break;
                    default:
                        /* NOTE: "\uHHHH" is unsupported for now. */
                        fprintf(stderr, "Unknown escape sequence: \"\\%c\"\n",
                                *str);
                        break;
                }
                break;
            default:
                /* Normal non-escapable character */
                ret[i++] = *str;
                break;
            case '\"':
            case '\0':
            case EOF:
                /* We are done with the string */
                done = true;
                break;
        }

        str++;
    }

    *length = i;
    return ret;
}

int main(void) {
    const char* real = "\x1f\123\034\t\n\a\b\e\f\n\r\t\v\\done\"";
    for (int i = 0; real[i] != '\0'; i++)
        printf("%02X ", real[i]);
    putchar('\n');

    /* Should output the same */
    const char* str =
      "\\x1f\\123\\034\\t\\n\\a\\b\\e\\f\\n\\r\\t\\v\\\\done\\\"\"";
    size_t length  = 0;
    uint8_t* bytes = str2bytes(str, &length);
    for (size_t i = 0; i < length; i++)
        printf("%02X ", bytes[i]);
    putchar('\n');

    return 0;
}
