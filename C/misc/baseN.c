/*
 * Copyright 2025 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/*
 * Maximum length of a string big enough to contain a byte of information in the
 * lowest posible base.
 *
 *     log2(UCHAR_MAX+1) == log2(256) == 8
 *
 * The actual length for an arbitrary base will be:
 *
 *     log(BASE, UCHAR_MAX+1)
 *       == log2(UCHAR_MAX+1) / log2(BASE)
 *       == log2(256) / log2(BASE)
 *       == 8.0 / log2(BASE)
 */
#define MAX_ENCODED_STR_LEN 8

/*
 * Reverse LEN characters of a string STR in place.
 */
static inline void reverse_in_place(char* str, size_t len) {
    char* p1 = str;
    char* p2 = &str[len - 1];
    for (; p2 > p1; p1++, p2--) {
        const char tmp = *p1;
        *p1            = *p2;
        *p2            = tmp;
    }
}

/*
 * Convert each byte in the input to base N and print it, where N is the length
 * of the specified alphabet.
 *
 * Try the program with:
 *
 *   echo "0: 01 02 AA BB FF" | xxd -r | ./baseN.out "01"
 *     => 00000001 00000010 10101010 10111011 11111111
 *
 *   echo "0: 01 02 AA BB FF" | xxd -r | ./baseN.out "0123456789abcdef"
 *     => 01 02 aa bb ff
 */
int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s ALPHABET < INPUT\n", argv[0]);
        return 1;
    }

    FILE* src = stdin;
    FILE* dst = stdout;

    const char* alphabet      = argv[1];
    const size_t alphabet_len = strlen(alphabet);
    if (alphabet_len < 2) {
        fprintf(stderr, "Invalid alphabet length (%zu<2).\n", alphabet_len);
        return 1;
    }

    static char encoded[MAX_ENCODED_STR_LEN];

    /*
     * Maximum position inside 'encoded' when using the specified
     * 'alphabet_len'. See comment in 'MAX_ENCODED_STR_LEN'.
     */
    const size_t max_encoded_pos =
      (size_t)ceil(8.0 / log2((double)alphabet_len));
    assert(max_encoded_pos <= MAX_ENCODED_STR_LEN);

    int c;
    while ((c = fgetc(src)) != EOF) {
        c &= 0xFF;

        for (size_t i = 0; i < max_encoded_pos; i++) {
            encoded[i] = alphabet[c % alphabet_len];
            c /= alphabet_len;
        }

        reverse_in_place(encoded, max_encoded_pos);
        fwrite(encoded, sizeof(char), max_encoded_pos, dst);

#ifdef SEPARATE_DECODED_BYTES
        fputc(' ', dst);
#endif
    }

#ifdef SEPARATE_DECODED_BYTES
    fputc('\n', dst);
#endif

    return 0;
}
