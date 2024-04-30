#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define KEY_LEN 20

/* (KEY_LEN * 2) + '\0' */
#define STR_KEY_LEN 41

static void generate_key(const char* user, uint8_t* out) {
    const int user_len = strlen(user);

    int key_pos          = 0;
    unsigned char last_c = 0;

    /* Iterate the user KEY_LEN+1 times */
    for (int iter = 0; iter < KEY_LEN + 1; iter++) {
        /* Iterate the user */
        for (int user_pos = 0; user[user_pos] != '\0'; user_pos++) {
            unsigned char c = user[user_pos];

            /* Swap every bit pair with the adjactent pair */
            c = ((c & 0x33333333) << 2) | ((c & 0xCCCCCCCC) >> 2);

            /* Swap every bit with the adjactent one */
            c = ((c & 0x55555555) << 1) | ((c & 0xAAAAAAAA) >> 1);

            /* XOR by the previous char */
            c ^= (last_c * iter) % 0xFF;

            /* Depending on the length of the input, change the output */
            c ^= user_len & 0xFF;

            /* Save current character for next iteration */
            last_c = c;

            /* Write the char to the current position */
            out[key_pos++] = c;

            /* Don't overflow the key */
            if (key_pos >= KEY_LEN)
                key_pos = 0;
        }
    }
}

/* 'f' -> 15 */
static inline int8_t char2nibble(char c) {
    if (c >= '0' && c <= '9')
        return c - '0';
    else if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    else if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    else
        return -1;
}

static void read_key(uint8_t* out) {
    int out_pos = 0;

    char key[STR_KEY_LEN] = { 0 };
    scanf("%41s", key);

    /* "3f5d" -> { 0x3f, 0x5d } */
    for (int i = 0; key[i] != '\0'; i++) {
        /* First half of the byte: '3' -> 3 */
        int8_t nibble = char2nibble(key[i]);
        if (nibble == -1)
            break;
        out[out_pos] = nibble;

        i++;
        if (key[i] == '\0')
            break;

        /* Move previous bits to higher half of byte */
        out[out_pos] <<= 4;

        /* Second half of the byte: 'f' -> 15 */
        nibble = char2nibble(key[i]);
        if (nibble == -1)
            break;
        out[out_pos] |= nibble;

        out_pos++;
    }
}

int main(void) {
    printf("Username: ");
    char user[255] = { 0 };
    scanf("%255s", user);

    printf("Key: ");
    uint8_t user_key[KEY_LEN] = { 0 };
    read_key(user_key);

    uint8_t real_key[KEY_LEN] = { 0 };
    generate_key(user, real_key);

    bool match = memcmp(user_key, real_key, KEY_LEN) == 0;

    if (match)
        puts("Correct key.");
    else
        puts("Invalid key.");

    return 0;
}
