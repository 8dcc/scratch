
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define KEY_LEN 20

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

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <username>\n", argv[0]);
        return 1;
    }

    const char* username = argv[1];
    uint8_t key[KEY_LEN] = { 0 };

    generate_key(username, key);

    printf("Generated key for \"%s\": ", username);
    for (int i = 0; i < KEY_LEN; i++)
        printf("%02x", key[i]);
    putchar('\n');

    return 0;
}
