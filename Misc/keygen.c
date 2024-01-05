
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define KEY_LEN 20

static void generate_key(const char* user, uint8_t* out) {
    int user_pos   = 0;
    bool user_done = false;

    for (int i = 0; i < KEY_LEN; i++) {
        unsigned char c = user[user_pos];

        /* Swap every bit pair with the adjactent pair */
        c = ((c & 0x55555555) << 1) | ((c & 0xAAAAAAAA) >> 1);

        /* XOR the first character by 0x33, and the rest by the previous one */
        c ^= (i == 0) ? 0x33 : out[i - 1];

        /* Depending on i, change the output a bit */
        if (i % 2 == 0)
            c ^= 1;

        /* Write the char to the current position */
        out[i] = c;

        /* If there are no chars left in the input, go to the begining again */
        user_pos++;
        if (user[user_pos] == '\0') {
            user_done = true;
            user_pos  = 0;
        }
    }

    /* We still have characters left in the user */
    if (!user_done) {
        int key_pos = 0;
        while (user[user_pos] != '\0') {
            out[key_pos++] ^= user[user_pos++];

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
