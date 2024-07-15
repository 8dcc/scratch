/* https://cryptopals.com/sets/1/challenges/5 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*----------------------------------------------------------------------------*/
/* Misc util functions */

static size_t get_file_sz(FILE* fp) {
    fseek(fp, 0L, SEEK_END);
    const size_t file_sz = ftell(fp);
    fseek(fp, 0L, SEEK_SET);

    return file_sz;
}

static char* read_file(FILE* fp, size_t file_sz) {
    char* content = malloc(file_sz);

    for (size_t i = 0; i < file_sz; i++)
        content[i] = fgetc(fp);

    return content;
}

static void print_bytes(uint8_t* bytes, size_t sz) {
    while (sz-- > 0)
        printf("%02X", *bytes++);
    putchar('\n');
}

/*----------------------------------------------------------------------------*/
/* XOR functions */

static uint8_t* repeating_xor(const char* key, const char* input,
                              size_t input_sz) {
    const size_t key_sz = strlen(key);

    uint8_t* result = malloc(input_sz);

    /* NOTE: I am not sure if doing a modulo is cheaper than resetting a
     * `key_pos' variable, but I think it looks cleaner this way. */
    for (size_t i = 0; i < input_sz; i++)
        result[i] = input[i] ^ key[i % key_sz];

    return result;
}

/*----------------------------------------------------------------------------*/
/* Main function */

int main(int argc, char** argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s \"key\" input.txt\n", argv[0]);
        return 1;
    }

    const char* key      = argv[1];
    const char* filename = argv[2];

    FILE* fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Failed to open file: '%s'\n", filename);
        return 1;
    }

    const size_t file_sz = get_file_sz(fp);
    char* file_content   = read_file(fp, file_sz);
    fclose(fp);

    uint8_t* result = repeating_xor(key, file_content, file_sz);
    print_bytes(result, file_sz);

    free(result);
    free(file_content);
    return 0;
}
