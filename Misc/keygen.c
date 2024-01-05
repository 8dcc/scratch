
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define KEY_LEN 20

static void generate_key(const char* user, uint8_t* out) {
    const int user_len = strlen(user);
    int user_pos       = 0;

    for (int i = 0; i < KEY_LEN; i++) {
        unsigned char c = user[user_pos];

        /* Swap every bit pair with the adjactent pair */
        c = ((c & 0x33333333) << 2) | ((c & 0xCCCCCCCC) >> 2);

        /* Swap every bit with the adjactent one */
        c = ((c & 0x55555555) << 1) | ((c & 0xAAAAAAAA) >> 1);

        /* XOR the first character by 0x33, and the rest by the previous one */
        c ^= (i == 0) ? 0x33 : out[i - 1];

        /* Write the char to the current position */
        out[i] = c;

        /* If there are no chars left in the input, go to the begining again */
        user_pos++;
        if (user[user_pos] == '\0')
            user_pos = 0;
    }

    /* XOR the output with the key N-1 times more */
    int key_pos = 0;
    for (int i = 0; i < user_len - 1; i++) {
        user_pos = 0;
        while (user[user_pos] != '\0') {
            out[key_pos] ^= user[user_pos];

            /* Depending on i and the length of the input, change the output */
            if (user_pos % 2 == 0)
                out[key_pos] ^= user_len & 0xFF;

            user_pos++;
            key_pos++;

            /* Don't overflow the key if the user is bigger than KEY_LEN*2 */
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
