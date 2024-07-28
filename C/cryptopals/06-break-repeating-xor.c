/* https://cryptopals.com/sets/1/challenges/6 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> /* tolower() */

#include "base64.h" /* ByteArray, base64* */

#define KEYSIZE_MIN 2
#define KEYSIZE_MAX 40

#define MIN(A, B) (((A) < (B)) ? (A) : (B))
#define MAX(A, B) (((A) > (B)) ? (A) : (B))

/*----------------------------------------------------------------------------*/
/* Misc util functions */

static char* read_file(FILE* fp) {
    fseek(fp, 0L, SEEK_END);
    const size_t file_sz = ftell(fp);
    fseek(fp, 0L, SEEK_SET);

    char* content = malloc(file_sz + 1);

    size_t i;
    for (i = 0; i < file_sz; i++)
        content[i] = fgetc(fp);
    content[i] = '\0';

    return content;
}

static void print_bytes(uint8_t* bytes, size_t sz) {
    while (sz-- > 0)
        printf("%02X ", *bytes++);
    putchar('\n');
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

static size_t guess_keysize(ByteArray bytes) {
    size_t result = 0;

    /* Calculate the most likely keysize based on the hamming distance */
    double best_distance = 0;
    for (size_t keysize = KEYSIZE_MIN;
         keysize <= KEYSIZE_MAX && (keysize * 2) <= bytes.size; keysize++) {
        /* Used for calculating the average (normalized) hamming distance
         * between all adjacent pairs of chunks. */
        double accumulated_distance = 0;
        int num_accumulated         = 0;

        /* Iterate all the possible key-sized chunks of data, and calculate
         * their normalized hamming distance. */
        for (size_t chunk = 0; (chunk + keysize * 2) < bytes.size;
             chunk += keysize * 2) {
            /* Calculate the hamming distance between the current chunk and the
             * adjacent one. */
            const double distance =
              hamming_distance(&bytes.data[chunk], &bytes.data[chunk + keysize],
                               keysize);

            /* Normalize it based on the keysize */
            const double normalized = (double)distance / keysize;

            /* Accumulate the distance and count for calculating the average
             * later. */
            accumulated_distance += normalized;
            num_accumulated++;
        }

        /* Calculate the average distance */
        const double average_distance = accumulated_distance / num_accumulated;

        /* Check if the average distance for this keysize is better (smaller)
         * than the one we had stored. If so, store this keysize as the best
         * one. */
        if (result == 0 || average_distance < best_distance) {
            best_distance = average_distance;
            result        = keysize;
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

    if (byte == ' ')
        return 1;

    return -2;
}

static uint8_t guess_byte_xor(uint8_t* bytes, size_t sz) {
    static const char possible_keys[] = "abcdefghijklmnopqrstuvwxyz"
                                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "0123456789"
                                        " !\"\'#$%&().,/\\:;";

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

static uint8_t* guess_key(ByteArray bytes, size_t keysize) {
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
    const bool has_partial_row = bytes.size % keysize != 0;
    size_t rows                = bytes.size / keysize;
    if (has_partial_row)
        rows++;

    /* Transpose the each block of `keysize' bytes. In other words, save each
     * table column in an array. */
    uint8_t** columns = (uint8_t**)malloc(keysize * sizeof(uint8_t*));
    for (size_t i = 0; i < keysize; i++) {
        columns[i] = (uint8_t*)malloc(rows);

        for (size_t j = 0; j < rows; j++) {
            columns[i][j] = bytes.data[keysize * j + i];
        }
    }

    /* Try to guess the byte used for XOR'ing each column of the table. */
    uint8_t* result_key = malloc(keysize);
    for (size_t i = 0; i < keysize; i++) {
        size_t column_size = rows;

        /* If the data in this column doesn't reach the last row, ignore the
         * last value. For example, in the table above, k0 has 4 bytes but k1
         * has only 3. */
        if (has_partial_row && i + 1 > bytes.size % keysize)
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
    char* file_content = read_file(fp);
    fclose(fp);

    /* Decode the file contents into an array of bytes. This is defined in the
     * `base64.h' header. */
    ByteArray decoded_bytes = base64_decode(file_content);
    free(file_content);
    printf("Number of decoded base64 bytes: %ld\n", decoded_bytes.size);

    /* Guess the keysize based on the hamming distance */
    const size_t keysize = guess_keysize(decoded_bytes);
    if (keysize == 0) {
        fprintf(stderr, "Error guessing keysize.\n");
        return 1;
    }
    printf("Most likely keysize: %ld\n", keysize);

    /* Guess the key itself. See comment in `guess_key' for more information. */
    uint8_t* key = guess_key(decoded_bytes, keysize);
    printf("Guessed key (bytes): ");
    print_bytes(key, keysize);
    printf("Guessed key (chars): ");
    for (size_t i = 0; i < keysize; i++)
        printf("%c", key[i]);
    printf("\n\n");

    printf("Decrypted:\n"
           "==========\n");
    for (size_t i = 0; i < decoded_bytes.size; i++)
        printf("%c", decoded_bytes.data[i] ^ key[i % keysize]);
    putchar('\n');

    free(decoded_bytes.data);
    free(key);
    return 0;
}
