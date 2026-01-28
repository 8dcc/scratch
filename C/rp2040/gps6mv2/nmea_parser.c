/*
 * Copyright 2026 8dcc
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
 */

#include "nmea_parser.h"

#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <ctype.h>

#define NMEA_START_DELIMITER    '$'
#define NMEA_CHECKSUM_DELIMITER '*'
#define NMEA_FIELD_DELIMITER    ','

static uint8_t hex_char_to_nibble(char c) {
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    return c - 'a' + 10;
}

bool nmea_parse_message(nmea_message_t* dst, const char* src, size_t src_sz) {
    assert(dst != NULL && src != NULL && src_sz > 0);

    /* Reset destination structure */
    dst->num_fields          = 0;
    dst->received_checksum   = 0x00;
    dst->calculated_checksum = 0xFF;

    /*
     * Calculated checksum of the payload, excluding the start delimiter ('$')
     * and the checksum delimiter ('*').
     */
    uint8_t calculated_checksum = 0;

    /* Current position in the current field we are reading */
    size_t cur_field_pos = 0;

    /* Current position in the source string */
    size_t src_pos = 0;

    /* Skip until the start delimiter */
    while (src[src_pos++] != NMEA_START_DELIMITER)
        if (src_pos >= src_sz)
            return false;

    /* Store the start pointer of the first field */
    dst->fields[0].data = &src[src_pos];

    while (src_pos < src_sz) {
        switch (src[src_pos]) {
            case NMEA_START_DELIMITER:
                fprintf(stderr, "Read start delimiter within a field.\n");
                return false;

            case NMEA_CHECKSUM_DELIMITER: {
                /* Skip checksum delimiter */
                src_pos++;

                if (src_pos + 2 >= src_sz) {
                    fprintf(stderr, "Not enough space for checksum.\n");
                    return false;
                }

                if (!isxdigit(src[src_pos]) || !isxdigit(src[src_pos + 1])) {
                    fprintf(stderr, "Received non-hex checksum.\n");
                    return false;
                }

                /* Convert hex characters to checksum byte */
                uint8_t upper_nibble = hex_char_to_nibble(src[src_pos++]);
                uint8_t lower_nibble = hex_char_to_nibble(src[src_pos]);
                dst->received_checksum = (upper_nibble << 4) | lower_nibble;

                /* Store the calculated checksum, since it ends here */
                dst->calculated_checksum = calculated_checksum;
            } break;

            case NMEA_FIELD_DELIMITER: {
                /* Store the length of the field we just finished reading */
                dst->fields[dst->num_fields].length = cur_field_pos;

                /* Don't fill the next field if we are done with the input */
                if (src_pos + 1 >= src_sz)
                    break;

                /* Move to the next field */
                dst->num_fields++;
                if (dst->num_fields >= NMEA_MAX_FIELDS) {
                    fprintf(stderr,
                            "Not enough fields (>%d).\n",
                            NMEA_MAX_FIELDS);
                    return false;
                }

                /* Store the initial pointer of the next field */
                dst->fields[dst->num_fields].data   = &src[src_pos + 1];
                dst->fields[dst->num_fields].length = 0;
                cur_field_pos                       = 0;
            } break;

            default: {
                cur_field_pos++;
            } break;
        }

        /*
         * The calculated checksum is the bit-wise XOR of each payload byte,
         * including field separators, but excluding the start and checksum
         * delimiters.
         */
        calculated_checksum ^= src[src_pos];

        src_pos++;
    }

    return true;
}

void nmea_print_escaped(const char* msg, size_t msg_sz) {
    for (size_t i = 0; i < msg_sz; i++) {
        const char c = msg[i];
        switch (c) {
            case '\n':
                printf("\\n");
                break;
            case '\r':
                printf("\\r");
                break;
            default:
                putchar(c);
                break;
        }
    }
}
