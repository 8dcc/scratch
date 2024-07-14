/* https://cryptopals.com/sets/1/challenges/6 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define KEYSIZE_MIN 2
#define KEYSIZE_MAX 40

#define MIN(A, B) (((A) < (B)) ? (A) : (B))
#define MAX(A, B) (((A) > (B)) ? (A) : (B))

/*----------------------------------------------------------------------------*/
/* Misc util functions */

static void print_bytes(uint8_t* bytes, size_t sz) {
    while (sz-- > 0)
        printf("%02X", *bytes++);
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
/* Crypto functions */

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

static int guess_keysize(uint8_t* bytes, size_t bytes_sz) {
    int result = -1;

    /* Calculate the most likely keysize based on the hamming distance */
    uint32_t min_distance = 0;
    for (size_t keysize = KEYSIZE_MIN;
         keysize <= KEYSIZE_MAX && (keysize * 2) <= bytes_sz; keysize++) {
        const uint32_t distance =
          hamming_distance(&bytes[0], &bytes[keysize], keysize);
        const uint32_t normalized = distance / keysize;

        if (result == -1 || normalized < min_distance) {
            min_distance = normalized;
            result       = keysize;
        }
    }

    return result;
}

static uint8_t* repeating_xor(const char* key, const char* input,
                              size_t input_sz) {
    const size_t key_sz = strlen(key);

    uint8_t* result = malloc(input_sz);

    /* NOTE: I am not sure if doing a modulo is cheaper than resetting a
     * `key_pos' variable, but I think it looks cleaner this way. */
    for (size_t i = 0; i < input_sz; i++)
        result[i] = input[i] ^ key[(i % key_sz)];

    return result;
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
    const int keysize = guess_keysize(decoded_bytes, decoded_bytes_sz);
    if (keysize == -1) {
        fprintf(stderr, "Error guessing keysize.\n");
        return 1;
    }

    printf("Most likely keysize: %d\n", keysize);

    /* TODO: Separate into keysize blocks, transpose into other blocks */

    free(decoded_bytes);
    return 0;
}
