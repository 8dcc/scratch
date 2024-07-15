/* https://cryptopals.com/sets/1/challenges/6 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> /* tolower() */

#define KEYSIZE_MIN 2
#define KEYSIZE_MAX 40

#define MIN(A, B) (((A) < (B)) ? (A) : (B))
#define MAX(A, B) (((A) > (B)) ? (A) : (B))

/*----------------------------------------------------------------------------*/
/* Misc util functions */

static void print_bytes(uint8_t* bytes, size_t sz) {
    while (sz-- > 0)
        printf("%02X ", *bytes++);
    putchar('\n');
}

/*----------------------------------------------------------------------------*/
/* Base64 functions */

static bool base64_valid_char(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
           (c >= '0' && c <= '9') || c == '+' || c == '/' || c == '=';
}

static char* base64_read_file(FILE* fp) {
    size_t content_sz = 100;
    char* content     = malloc(content_sz);

    int c;
    size_t i = 0;
    while ((c = fgetc(fp)) != EOF) {
        if (!base64_valid_char(c))
            continue;

        if (i >= content_sz) {
            content_sz += 100;
            content = realloc(content, content_sz);
        }

        content[i++] = c;
    }

    if (i >= content_sz)
        content = realloc(content, content_sz + 1);

    content[i] = '\0';
    return content;
}

static size_t base64_byte_count(size_t num_chars) {
    /* Each 4 characters in base64 represent 3 bytes */
    return num_chars / 4 * 3;
}

/* Return the 6 bits represented by the specified base64 character */
static uint8_t base64_char2bits(char c) {
    if (c >= 'A' && c <= 'Z')
        return c - 'A';

    if (c >= 'a' && c <= 'z')
        return c - 'a' + 26;

    if (c >= '0' && c <= '9')
        return c - '0' + 52;

    if (c == '+')
        return 62;

    if (c == '/')
        return 63;

    fprintf(stderr, "%s: Invalid base64 character: '%c'\n", __func__, c);
    exit(1);
}

/* Decodes the null-terminated base64 `src' string into the `dst' byte array.
 * Returns the number of written bytes.
 *
 * The `dst' array must be allocated and freed by the caller, and must be big
 * enough to hold the bytes. To get the size, use `base64_byte_count'. */
static size_t base64_decode(uint8_t* dst, const char* src) {
    uint8_t bits;

    /* Assumes the length of `src' is a multiple of 4, which should be always
     * true because of base64 padding. */
    size_t i = 0;
    while (*src != '\0') {
        /* Bits 2..8 of first byte */
        bits   = base64_char2bits(*src++);
        dst[i] = bits << 2;

        /* Bits 0..2 of first byte */
        bits = base64_char2bits(*src);
        dst[i++] |= bits >> 4;

        /* The third base64 char might be padding */
        src++;
        if (*src == '=')
            break;

        /* Bits 4..8 of second byte */
        dst[i] = (bits & 0xF) << 4;

        /* Bits 0..3 of second byte */
        bits = base64_char2bits(*src);
        dst[i++] |= bits >> 2;

        /* Bits 7..8 of third byte */
        dst[i] = (bits & 0x3) << 6;

        /* The fourth base64 char might be padding */
        src++;
        if (*src == '=')
            break;

        /* Bits 0..6 of fourth byte */
        bits = base64_char2bits(*src++);
        dst[i++] |= bits;
    }

    return i;
}

/*----------------------------------------------------------------------------*/
/* Key size guessing functions */

/* For a more complete example, see C/algorithms/hamming-distance.c */
static uint32_t hamming_distance(const uint8_t* a, const uint8_t* b,
                                 size_t sz) {
    uint32_t result = 0;

    /* Only compare up to the length of the smallest string */
    for (size_t i = 0; i < sz; i++) {
        const uint8_t byte_a = a[i];
        const uint8_t byte_b = b[i];

        /* Iterate each bit */
        for (int j = 0; j < 8; j++) {
            const bool bit_a = (byte_a >> j) & 1;
            const bool bit_b = (byte_b >> j) & 1;

            /* Add one to the hamming distance if they don't match */
            if (bit_a != bit_b)
                result++;
        }
    }

    return result;
}

static size_t guess_keysize(uint8_t* data, size_t data_sz) {
    size_t result = 0;

    /* Calculate the most likely keysize based on the hamming distance */
    uint32_t min_distance = 0;
    for (size_t keysize = KEYSIZE_MIN;
         keysize <= KEYSIZE_MAX && (keysize * 2) <= data_sz; keysize++) {
        const uint32_t distance =
          hamming_distance(&data[0], &data[keysize], keysize);
        const uint32_t normalized = distance / keysize;

        if (result == 0 || normalized < min_distance) {
            min_distance = normalized;
            result       = keysize;
        }
    }

    return result;
}

/*----------------------------------------------------------------------------*/
/* XOR and key ranking functions */

/* Arbitrary function for determining how likely is a byte of being a string */
static int char_score(uint8_t byte) {
    if ((byte >= 'a' && byte <= 'z') || (byte >= 'A' && byte <= 'Z')) {
        char c = tolower((char)byte);

        /*
         * https://www3.nd.edu/~busiforc/handouts/cryptography/letterfrequencies.html
         */
        if (c == 'e' || c == 'a' || c == 'r' || c == 'i' || c == 'o' ||
            c == 't' || c == 'n' || c == 's' || c == 'l' || c == 'c')
            return 3;

        if (c == 'u' || c == 'd' || c == 'p' || c == 'm' || c == 'h' ||
            c == 'g' || c == 'b' || c == 'f' || c == 'y' || c == 'w')
            return 2;

        if (c == 'k' || c == 'v' || c == 'x' || c == 'z' || c == 'j' ||
            c == 'q')
            return 1;
    }

    if (byte >= '0' && byte <= '9')
        return 0;

    return -2;
}

static uint8_t guess_byte_xor(uint8_t* bytes, size_t sz) {
    static const char possible_keys[] =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    int best_score = 0;
    uint8_t result = 0;

    for (size_t i = 0; i < sizeof(possible_keys); i++) {
        /* Overall "score" of the current key for the entire input */
        int cur_key_score = 0;

        /* XOR each character of the input with the current key, and accumulate
         * the "character score". */
        for (size_t j = 0; j < sz; j++) {
            const char xor = bytes[j] ^ possible_keys[i];
            cur_key_score += char_score(xor);
        }

        /* If the score for the current key is better than the one we had, save
         * the current key as the best one for now. */
        if (cur_key_score > best_score) {
            best_score = cur_key_score;
            result     = possible_keys[i];
        }
    }

    return result;
}

static uint8_t* guess_key(const uint8_t* data, size_t data_sz, size_t keysize) {
    /*
     * Let's assume this is the decoded data we received.
     *
     *   0123456789abcdef
     *
     * If we know that the keysize is 5, since a repeating-key XOR cypher has
     * been used, we know that the same key has been re-used for each group of 5
     * bytes.
     *
     *   01234
     *   56789
     *   abcde
     *   f....
     *
     * We can also tell that, for each position in each group, the same
     * byte of the key has been used when performing the bit-wise XOR.
     *
     *   | k0 | k1 | k2 | k3 | k4 |
     *   |----+----+----+----+----|
     *   | 0  | 1  | 2  | 3  | 4  |
     *   | 5  | 6  | 7  | 8  | 9  |
     *   | a  | b  | c  | d  | e  |
     *   | f  | .  | .  | .  | .  |
     *
     * Where k0..k4 represents each byte of the key. With this in mind, we can
     * treat each column as a single-byte XOR'd string, and use the same code
     * from Challenge 3.
     *
     * First, calculate the number of data chunks (table rows).
     */
    const bool has_partial_row = data_sz % keysize != 0;
    size_t rows                = data_sz / keysize;
    if (has_partial_row)
        rows++;

    /* Transpose the each block of `keysize' bytes. In other words, save each
     * table column in an array. */
    uint8_t** columns = (uint8_t**)malloc(keysize * sizeof(uint8_t*));
    for (size_t i = 0; i < keysize; i++) {
        columns[i] = (uint8_t*)malloc(rows);

        for (size_t j = 0; j < rows; j++) {
            columns[i][j] = data[keysize * i + j];
        }
    }

    /* Try to guess the byte used for XOR'ing each column of the table. */
    uint8_t* result_key = malloc(keysize);
    for (size_t i = 0; i < keysize; i++) {
        size_t column_size = rows;

        /* If the data in this column doesn't reach the last row, ignore the
         * last value. For example, in the table above, k0 has 4 bytes but k1
         * has only 3. */
        if (has_partial_row && i + 1 > data_sz % keysize)
            column_size--;

        result_key[i] = guess_byte_xor(columns[i], column_size);
    }

    /* Free each column and the columns array itself */
    for (size_t i = 0; i < keysize; i++)
        free(columns[i]);
    free(columns);

    return result_key;
}

/*----------------------------------------------------------------------------*/
/* Main function */

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s input.txt\n", argv[0]);
        return 1;
    }

    const char* filename = argv[1];

    FILE* fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Failed to open file: '%s'\n", filename);
        return 1;
    }

    /* Read the base64 characters from the file */
    char* base64_chars = base64_read_file(fp);
    fclose(fp);

    /* NOTE: Call to `strlen' could be avoided by returning the bytes from
     * `base64_read_file', but I think it would be more confusing. */
    uint8_t* decoded_bytes = malloc(base64_byte_count(strlen(base64_chars)));
    const size_t decoded_bytes_sz = base64_decode(decoded_bytes, base64_chars);
    free(base64_chars);
    printf("Number of decoded base64 bytes: %ld\n", decoded_bytes_sz);

    /* Guess the keysize based on the hamming distance */
    const size_t keysize = guess_keysize(decoded_bytes, decoded_bytes_sz);
    if (keysize == 0) {
        fprintf(stderr, "Error guessing keysize.\n");
        return 1;
    }
    printf("Most likely keysize: %ld\n", keysize);

    uint8_t* key = guess_key(decoded_bytes, decoded_bytes_sz, keysize);
    printf("Guessed key (bytes): ");
    print_bytes(key, keysize);

    printf("Decrypted: ");
    for (size_t i = 0; i < decoded_bytes_sz; i++)
        printf("%c", decoded_bytes[i] ^ key[i % keysize]);

    free(decoded_bytes);
    free(key);
    return 0;
}
