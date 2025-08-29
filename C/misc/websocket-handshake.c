/*
 * Copyright 2025 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 *
 * ----------------------------------------------------------------------------
 *
 * This program depends on OpenSSL and 'libb64'. Compile and link with:
 *   $ gcc -o websocket-handshake.out websocket-handshake.c -lcrypto -lb64
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <openssl/sha.h>
#include <b64/cencode.h>

/*
 * WebSocket Globally Unique Identifier, used for building the handshake key.
 */
#define WS_GUID "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

/*
 * Return the compile-time length of a string.
 */
#define STRLEN(STR) (sizeof(STR) - 1)

/*
 * DEBUG: Dumps an array with compile-time known length.
 */
#define DUMP(ARR, LEN)                                                         \
    do {                                                                       \
        for (size_t i = 0; i < LEN; i++)                                       \
            printf("%02X ", ((uint8_t*)ARR)[i] & 0xFF);                        \
        putchar('\n');                                                         \
    } while (0)

/*
 * Generate a base64 handshake response from the input base64 client key.
 *
 * From RFC 64455:
 *
 *   > For this header field, the server has to take the value (as present in
 *   > the header field, e.g., the base64-encoded version minus any leading and
 *   > trailing whitespace) and concatenate this with the Globally Unique
 *   > Identifier (GUID) "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in string form,
 *   > which is unlikely to be used by network endpoints that do not understand
 *   > the WebSocket Protocol. A SHA-1 hash (160 bits), base64-encoded, of this
 *   > concatenation is then returned in the server's handshake.
 *
 * Example input:
 *
 *   dGhlIHNhbXBsZSBub25jZQ==
 *
 * Expected output:
 *
 *   s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
 */
static bool get_websocket_handshake_key(const char* base64_client_key,
                                        char* output,
                                        size_t output_sz) {
    /*
     * The maximum size of this buffer is 60, that is, the sum of the
     * base64-encoded client key (24) and the length of the fixed WebSocket
     * GUID (36).
     */
    char concat_buffer[61];

    const size_t input_len         = strlen(base64_client_key);
    const size_t concat_buffer_len = input_len + STRLEN(WS_GUID);
    if (concat_buffer_len + 1 > sizeof(concat_buffer))
        return false;

    strncpy(concat_buffer, base64_client_key, input_len);
    strcpy(concat_buffer + input_len, WS_GUID);

    /* DEBUG */
    printf("Concatenated: ");
    DUMP(concat_buffer, sizeof(concat_buffer));

    uint8_t hash[SHA_DIGEST_LENGTH];
    SHA1((uint8_t*)concat_buffer, concat_buffer_len, hash);

    /* DEBUG */
    printf("SHA1: ");
    DUMP(hash, sizeof(hash));

    /* Every 3 bytes in the input translate to 4 encoded bytes */
    const size_t future_base64_len = 4 * ((sizeof(hash) + 2) / 3);
    if (future_base64_len + 1 > output_sz)
        return false;

    base64_encodestate b64_state;
    base64_init_encodestate(&b64_state);
    size_t base64_len =
      base64_encode_block((char*)hash, sizeof(hash), output, &b64_state);
    base64_len += base64_encode_blockend(output + base64_len, &b64_state);
    if (output[base64_len - 1] == '\n')
        output[base64_len - 1] = '\0';

    /* DEBUG */
    printf("Encoded: ");
    DUMP(output, base64_len);

    return true;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s CLIENT_KEY\n", argv[0]);
        return 1;
    }

    const char* client_key = argv[1];

    /*
     * The maximum response size is 28, that is, the length of the
     * base64-encoded SHA1 hash key.
     */
    char base64_response[29];
    if (!get_websocket_handshake_key(client_key,
                                     base64_response,
                                     sizeof(base64_response))) {
        fprintf(stderr, "<error>\n");
        return 1;
    }

    printf("%s\n", base64_response);
    return 0;
}
