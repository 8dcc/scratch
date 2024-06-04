/* https://www.cryptopals.com/sets/1/challenges/1 */

#include <stdio.h>
#include <stdlib.h>

/* From base64.asm */
extern char* base64(char* dst, const void* src, size_t sz);

/*
 * Each character is used to represent 6 bits, since: log2(64) = 6
 * With 4 characters, you can represent 24 bits (3 bytes)
 *
 * To represent N bytes, you need to divide the number of bytes by 3 (the number
 * of bytes represented with 4 chars), and then muliply that by the chars
 * required to represent those 3-byte groups (which we know to be 4 chars).
 *
 * Since the number of characters in a base64 string has to be divisible by 4,
 * we will align the input bytes to make sure it's divisible by 3.
 */
int base64_chars(size_t num_bytes) {
    if (num_bytes % 3 != 0)
        num_bytes += 3 - (num_bytes % 3);

    return 4 * num_bytes / 3;
}

int main(void) {
    const char bytes[] = { 0x49, 0x27, 0x6d, 0x20, 0x6b, 0x69, 0x6c, 0x6c, 0x69,
        0x6e, 0x67, 0x20, 0x79, 0x6f, 0x75, 0x72, 0x20, 0x62, 0x72, 0x61, 0x69,
        0x6e, 0x20, 0x6c, 0x69, 0x6b, 0x65, 0x20, 0x61, 0x20, 0x70, 0x6f, 0x69,
        0x73, 0x6f, 0x6e, 0x6f, 0x75, 0x73, 0x20, 0x6d, 0x75, 0x73, 0x68, 0x72,
        0x6f, 0x6f, 0x6d };

    const int encoded_chars = base64_chars(sizeof(bytes));
    char* encoded = malloc(encoded_chars + 1);
    base64(encoded, bytes, sizeof(bytes));

    /* SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t */
    printf("%s\n", encoded);

    free(encoded);
    return 0;
}
