/* https://cryptopals.com/sets/1/challenges/3 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> /* tolower() */

#define LENGTH(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

/*----------------------------------------------------------------------------*/
/* Misc util functions */

static bool hex2bytes(uint8_t* bytes, const char* str) {
    /* Each iteration, parse 2 characters of the input */
    for (int i = 0; *str != '\0'; i++) {
        uint8_t n = 0;

        if (*str >= '0' && *str <= '9')
            n = *str - '0';
        else if (*str >= 'a' && *str <= 'f')
            n = 10 + *str - 'a';
        else if (*str >= 'A' && *str <= 'F')
            n = 10 + *str - 'A';
        else
            return false;

        bytes[i] = (n & 0xF) << 4;
        str++;

        if (*str >= '0' && *str <= '9')
            n = *str - '0';
        else if (*str >= 'a' && *str <= 'f')
            n = 10 + *str - 'a';
        else if (*str >= 'A' && *str <= 'F')
            n = 10 + *str - 'A';
        else
            return false;

        bytes[i] |= n & 0xF;
        str++;
    }

    return true;
}

/*----------------------------------------------------------------------------*/
/* XOR ranking functions */

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

static char most_likely_key(uint8_t* bytes, size_t sz) {
    static const char possible_keys[] =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    int best_score = 0;
    char result    = 0;

    for (size_t i = 0; i < LENGTH(possible_keys); i++) {
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

/*----------------------------------------------------------------------------*/
/* Main function */

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <str>\n", argv[0]);
        return 1;
    }

    size_t bytes_sz = strlen(argv[1]) / 2;
    uint8_t* bytes  = malloc(bytes_sz);
    if (!hex2bytes(bytes, argv[1])) {
        fprintf(stderr, "Expected a string in hexadecimal format\n");
        return 1;
    }

    const char key = most_likely_key(bytes, bytes_sz);
    printf("Most likely key: '%c' (%d)\n"
           "Output: ", key, key);

    for (size_t i = 0; i < bytes_sz; i++)
        printf("%c", (char)(bytes[i] ^ key));
    putchar('\n');

    free(bytes);
    return 0;
}
