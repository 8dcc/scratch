/*
 * base64.h - Functions related to base64 encoding and decoding.
 * See: https://github.com/8dcc/scratch
 * Copyright (C) 2024 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef BASE64_H_
#define BASE64_H_ 1

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h> /* Error messages */
#include <stdlib.h>

typedef struct ByteArray {
    uint8_t* data;
    size_t size;
} ByteArray;

/*----------------------------------------------------------------------------*/

/*
 * Return the number of base64 characters that would be used to represent the
 * specified number of bytes.
 *
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
size_t base64_bytes2chars(size_t num_bytes) {
    if (num_bytes % 3 != 0)
        num_bytes += 3 - (num_bytes % 3);

    return 4 * num_bytes / 3;
}

/*
 * Return the number of bytes that would be represented by the specified number
 * of base64 characters. See also `base64_bytes2chars'.
 *
 * Each 4 characters in base64 represent 3 bytes.
 */
size_t base64_chars2bytes(size_t num_chars) {
    return num_chars / 4 * 3;
}

/* Return true if a character is a valid base64 input, including the padding
 * character and the null-terminator. */
bool base64_valid_char(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
           (c >= '0' && c <= '9') || c == '+' || c == '/' || c == '=' ||
           c == '\0';
}

/* Return the 6 bits represented by the specified base64 character */
uint8_t base64_char2bits(char c) {
    if (c >= 'A' && c <= 'Z')
        return c - 'A';

    if (c >= 'a' && c <= 'z')
        return c - 'a' + 26;

    if (c >= '0' && c <= '9')
        return c - '0' + 52;

    if (c == '+')
        return 62;

    if (c == '/')
        return 63;

    fprintf(stderr, "%s: Invalid base64 character: '%c'\n", __func__, c);
    abort();
}

/*----------------------------------------------------------------------------*/

/* Encode the specified bytes into an allocated buffer that must be freed by the
 * caller. */
char* base64_encode(const void* src, size_t sz) {
    static const char base64_chars[] =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    /* Encoded base64 string that will be returned */
    size_t encoded_sz  = 100;
    char* encoded      = (char*)malloc(encoded_sz);
    size_t encoded_pos = 0;

    /* Used for iterating the input data */
    const uint8_t* binary = (uint8_t*)src;

    /* Used to hold the 6 bits for accessing the `base64_chars' array */
    uint8_t base64_idx;

    while (sz > 0) {
        /* First, make sure we have space left for the next 4 characters plus a
         * potential null terminator. */
        if (encoded_pos + 5 >= encoded_sz) {
            encoded_sz += 100;
            encoded = realloc(encoded, encoded_sz);
        }

        /* Bits 2..7 of 1st input byte */
        base64_idx             = *binary >> 2;
        encoded[encoded_pos++] = base64_chars[base64_idx];

        sz--;
        if (sz <= 0) {
            encoded[encoded_pos++] = '=';
            encoded[encoded_pos++] = '=';
            break;
        }

        /* Bits 0..1 of 1st input byte, and bits 4..7 of 2nd input byte */
        base64_idx = (*binary << 4) & 0x3F;
        binary++;
        base64_idx |= *binary >> 4;
        encoded[encoded_pos++] = base64_chars[base64_idx];

        sz--;
        if (sz <= 0) {
            encoded[encoded_pos++] = '=';
            break;
        }

        /* Bits 0..3 of 2nd input byte, and bits 6..7 of 3rd input byte */
        base64_idx = (*binary << 2) & 0x3F;
        binary++;
        base64_idx |= *binary >> 6;
        encoded[encoded_pos++] = base64_chars[base64_idx];

        /* Bits 0..5 of 3rd input byte */
        base64_idx             = *binary & 0x3F;
        encoded[encoded_pos++] = base64_chars[base64_idx];
        binary++;
        sz--;
    }

    /* Null terminate the string */
    assert(encoded_pos < encoded_sz);
    encoded[encoded_pos] = '\0';

    return encoded;
}

/* Decode the null-terminated base64 string */
ByteArray base64_decode(const char* str) {
    ByteArray decoded;
    decoded.size = 100;
    decoded.data = malloc(decoded.size);

    /* Will be used to store each base64 character */
    uint8_t bits;

    /* Before we start decoding, make sure we are on the first base64 char */
    while (!base64_valid_char(*str))
        str++;

    size_t i = 0;
    while (*str != '\0') {
        /* First, make sure there is enough space on the destination array for
         * the next 3 bytes. */
        if (i + 2 >= decoded.size) {
            decoded.size += 100;
            decoded.data = realloc(decoded.data, decoded.size);
        }

        /* Bits 2..8 of first byte */
        bits            = base64_char2bits(*str);
        decoded.data[i] = bits << 2;

        /* Move to second base64 char */
        do {
            str++;
        } while (!base64_valid_char(*str));

        if (*str == '\0')
            break;

        /* Bits 0..2 of first byte */
        bits = base64_char2bits(*str);
        decoded.data[i++] |= bits >> 4;

        /* Move to third base64 char. Might be padding. */
        do {
            str++;
        } while (!base64_valid_char(*str));

        if (*str == '\0' || *str == '=')
            break;

        /* Bits 4..8 of second byte */
        decoded.data[i] = (bits & 0xF) << 4;

        /* Bits 0..3 of second byte */
        bits = base64_char2bits(*str);
        decoded.data[i++] |= bits >> 2;

        /* Move to fourth base64 char. Might be padding. */
        do {
            str++;
        } while (!base64_valid_char(*str));

        if (*str == '\0' || *str == '=')
            break;

        /* Bits 7..8 of third byte */
        decoded.data[i] = (bits & 0x3) << 6;

        /* Bits 0..6 of fourth byte */
        bits = base64_char2bits(*str);
        decoded.data[i++] |= bits;

        /* Move to first base64 char of next four-char group */
        do {
            str++;
        } while (!base64_valid_char(*str));
    }

    /* Actual data size, not what we re-allocated */
    decoded.size = i;
    return decoded;
}

#endif /* BASE64_H_ */
