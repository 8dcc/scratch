/*
 * XOR strings in a C source file with random key.
 * Usage: ./string-obfuscation.out < input.c > output.c
 *
 * TODO: Uses compound literals, so it produces a warning when using the -ansi
 * and -Wpedantic flags.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h> /* srand, rand */
#include <string.h> /* strlen */
#include <time.h>   /* time */

#define LENGTH(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

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

/* Consume and return a C string. Asumes the last call to `getchar' returned an
 * opening double-quote. */
static char* get_c_str(void) {
    size_t ret_sz = 100;
    char* ret     = calloc(ret_sz, sizeof(char));

    /* Skip over '\"' character */
    char c = getchar();

    for (size_t i = 0; c != '\"' && c != EOF; i++) {
        /* Reallocate output array if we ran out of space */
        if (i >= ret_sz) {
            ret_sz += 100;
            ret = realloc(ret, ret_sz);
        }

        ret[i] = c;

        /* Save the escaped character, and skip it. This is used to handle
         * escaped double-quotes. */
        if (c == '\\')
            ret[++i] = getchar();

        /* Get the next character */
        c = getchar();
    }

    return ret;
}

/* Print the function definition for `funcname'. It XORs a string using `key', a
 * `key_sz' long string. */
static void print_xor_func(const char* funcname, const uint8_t* key,
                           size_t key_sz) {
    printf("static const char* %s(char* str, unsigned long str_sz) {\n"
           "    unsigned long i, kp;\n"
           "    const unsigned char key[] = {",
           funcname);

    /* Print the key */
    for (size_t i = 0; i < key_sz; i++)
        printf("0x%02X,", key[i]);
    printf("};\n");

    printf("    for (i = 0, kp = 0; i < str_sz; i++, kp++) {\n"
           "        if (kp >= (unsigned long)sizeof(key))\n"
           "            kp = 0;\n"
           "        str[i] ^= key[kp];\n"
           "    }\n"
           "    return str;\n"
           "}\n\n");
}

/*----------------------------------------------------------------------------*/

int main(void) {
    /* TODO: Randomize function name */
    const char* strxor_funcname = "RandomStrXor";

    /* TODO: Randomize XOR key */
    const uint8_t xor_key[] = "ABC123XYZ";

    /* Print RandomStrXor function definition before the source */
    print_xor_func(strxor_funcname, xor_key, LENGTH(xor_key));

    int c;
    while ((c = getchar()) != EOF) {
        switch (c) {
            case '\"':
                /* Consume the C string, until closing '\"'. */
                char* c_str = get_c_str();

                /* Convert to array of bytes, parsing escaping sequences */
                size_t bytes_sz;
                uint8_t* bytes = str2bytes(c_str, &bytes_sz);

                /* Free raw C string allocated by `get_c_str'. */
                free(c_str);

                /* Wrap in a call to our XOR function */
                printf("%s((char[]){\"", strxor_funcname);

                /* XOR the bytes, and print them in hex format */
                for (size_t i = 0, kp = 0; i < bytes_sz; i++, kp++) {
                    if (kp >= LENGTH(xor_key))
                        kp = 0;

                    printf("\\x%02X", bytes[i] ^ xor_key[kp]);
                }

                /* Close the call to our XOR function, adding the length */
                printf("\"}, %ld)", bytes_sz);

                /* Free the bytes parsed by `str2bytes' */
                free(bytes);
                break;
            default:
                putchar(c);
                break;
        }
    }

    return 0;
}
