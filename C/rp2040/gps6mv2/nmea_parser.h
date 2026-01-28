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

#ifndef NMEA_PARSER_H_
#define NMEA_PARSER_H_ 1

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define NMEA_MAX_FIELDS 20

/*
 * Structure representing a single field of a NMEA message. The data must not be
 * null-terminated.
 */
typedef struct {
    const char* data;
    size_t length;
} nmea_field_t;

/*
 * Structure representing an entire parsed NMEA message.
 */
typedef struct {
    /* Array of data fields in the message */
    nmea_field_t fields[NMEA_MAX_FIELDS];
    size_t num_fields;

    /* Checksum received in the message itself */
    uint8_t received_checksum;

    /* Checksum calculated from the actual message data */
    uint8_t calculated_checksum;
} nmea_message_t;

/*
 * Parse a NMEA message into a 'nmea_message_t' structure.
 */
bool nmea_parse_message(nmea_message_t* dst, const char* src, size_t src_sz);

/*
 * Print a NMEA message, escaping special characters like '\n' and '\r'.
 */
void nmea_print_escaped(const char* msg, size_t msg_sz);

#endif /* NMEA_PARSER_H_ */
