
/*
 * Credits:
 *   - http://www.cse.yorku.ca/~oz/hash.html
 *     https://web.archive.org/web/20230714154454/http://www.cse.yorku.ca/~oz/hash.html
 *   - http://casualhacks.net/hashtool.html
 *     https://web.archive.org/web/20160102205932/http://www.casualhacks.net/hashtool.html
 *   - https://www.unknowncheats.me/forum/904026-post8.html
 *     https://web.archive.org/web/20140822095256/http://www.unknowncheats.me/forum/904026-post8.html
 */

#include <stdint.h>
#include <stdio.h>

#define BASE_HASH 5381
#define MAGIC_NUMBER 33

static inline uint32_t hash(const char* str) {
    uint32_t hashed = BASE_HASH;

    for (int i = 0; str[i] != '\0'; i++)
        hashed = (hashed * MAGIC_NUMBER) ^ str[i];

    return hashed;
}


int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s \"str\"\n", argv[0]);
        return 1;
    } 

    printf("hash(\"%s\"): 0x%X\n", argv[1], hash(argv[1]));
    return 0;
}
