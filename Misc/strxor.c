/*
 * XOR string with random key and print in C format.
 * Usage: ./strxor.out <str>
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h> /* srand, rand */
#include <string.h> /* strlen */
#include <time.h>   /* time */

#define COMMENT_START "/* XOR(\""
#define COMMENT_END   "\") */\n"
#define ARR1_STR      "char s1[] = \""
#define ARR2_STR      "char s2[] = \""
#define ARR_END       "\";\n"

int main(int argc, char** argv) {
    if (argc <= 1) {
        fprintf(stderr, "Usage: %s <str>\n", argv[0]);
        return 1;
    }

    /* Total size of the arguments. Add 1 each argv for the space. */
    size_t total_sz = 0;
    for (int i = 1; i < argc; i++)
        total_sz += strlen(argv[i]) + 1;
    total_sz--;

    /* Allocate arrays for the XOR'd string and the key */
    uint8_t* str = calloc(total_sz, sizeof(uint8_t));
    uint8_t* key = calloc(total_sz, sizeof(uint8_t));

    /* Fill key array with random bytes */
    srand(time(NULL));
    for (size_t i = 0; i < total_sz; i++)
        key[i] = rand() % 0xFF;

    /* XOR each char of argv and save in the allocated str array */
    int str_pos = 0;
    for (int i = 1; i < argc; i++) {
        for (int j = 0; argv[i][j] != '\0'; j++) {
            str[str_pos] = argv[i][j] ^ key[str_pos];
            str_pos++;
        }

        if (i < argc - 1) {
            str[str_pos] = ' ' ^ key[str_pos];
            str_pos++;
        }
    }

    /* Print comment with the XOR'd strings (Could print in the loop above) */
    printf("%s", COMMENT_START);
    for (int i = 1; i < argc; i++) {
        for (int j = 0; argv[i][j] != '\0'; j++)
            putchar(argv[i][j]);
        if (i < argc - 1)
            putchar(' ');
    }
    printf("%s", COMMENT_END);

    /* Print both arrays as C strings */
    printf("%s", ARR1_STR);
    for (size_t i = 0; i < total_sz; i++)
        printf("\\x%02X", str[i]);
    printf("%s", ARR_END);

    printf("%s", ARR2_STR);
    for (size_t i = 0; i < total_sz; i++)
        printf("\\x%02X", key[i]);
    printf("%s", ARR_END);

    return 0;
}

void example(void) {
    /* Output of main() */
    char s1[] = "\x7C\x2C\x19\x7D\x52\xC1\xBE\x50\xE7\x9D\xFC\x7A\xE9";
    char s2[] = "\x34\x49\x75\x11\x3D\xED\x9E\x27\x88\xEF\x90\x1E\xC8";

    /* Decrypt s1 in place */
    for (int i = 0; s1[i] != '\0'; i++)
        s1[i] ^= s2[i];

    /* Print decrypted */
    printf("%s\n", s1);
}
