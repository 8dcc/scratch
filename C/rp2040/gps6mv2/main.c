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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pico/stdlib.h"
#include "hardware/uart.h"
#include "hardware/irq.h"

#include "nmea_parser.h"

#if !defined(PICO_DEFAULT_LED_PIN) || PICO_DEFAULT_LED_PIN >= NUM_BANK0_GPIOS
#error Expected a valid LED pin.
#endif

/* GPS module configuration */
#define GPS_UART_ID     uart1
#define GPS_BAUD_RATE   9600
#define GPS_UART_TX_PIN 4
#define GPS_UART_RX_PIN 5

/* NMEA sentence buffer */
#define NMEA_BUFFER_SIZE 256

/* Ring buffer for UART RX (must be power of 2) */
#define RX_BUFFER_SIZE 512
static volatile char rx_buffer[RX_BUFFER_SIZE];
static volatile uint16_t rx_head = 0;
static volatile uint16_t rx_tail = 0;

/* UART RX interrupt handler */
static void on_uart_rx(void) {
    while (uart_is_readable(GPS_UART_ID)) {
        char c             = uart_getc(GPS_UART_ID);
        uint16_t next_head = (rx_head + 1) & (RX_BUFFER_SIZE - 1);
        if (next_head != rx_tail) {
            rx_buffer[rx_head] = c;
            rx_head            = next_head;
        }
        /* If buffer full, drop the character */
    }
}

/* Read a character from the RX ring buffer, returns -1 if empty */
static int rx_getc(void) {
    if (rx_head == rx_tail)
        return -1;
    char c  = rx_buffer[rx_tail];
    rx_tail = (rx_tail + 1) & (RX_BUFFER_SIZE - 1);
    return c;
}

/* GPS data structure */
typedef struct {
    char time[12];
    char latitude[16];
    char lat_dir[2];
    char longitude[16];
    char lon_dir[2];
    char altitude[12];
    int satellites;
    int fix_quality;
    char date[12];
    char speed[12];
    char course[12];
    bool data_valid;
} gps_data_t;

static gps_data_t gps_data = { 0 };

/* Copy field to destination buffer with size limit */
static void copy_field(char* dst, size_t dst_sz, const nmea_field_t* field) {
    size_t len = (field->length < dst_sz - 1) ? field->length : dst_sz - 1;
    memcpy(dst, field->data, len);
    dst[len] = '\0';
}

/* Parse integer from field (not null-terminated) */
static int parse_field_int(const nmea_field_t* field) {
    int result = 0;
    for (size_t i = 0; i < field->length; i++) {
        char c = field->data[i];
        if (c >= '0' && c <= '9')
            result = result * 10 + (c - '0');
        else
            break;
    }
    return result;
}

/* Parse GPGGA sentence (position and fix data) */
static void parse_gpgga(const nmea_message_t* msg) {
    /* Field 1: UTC time */
    if (msg->num_fields > 1 && msg->fields[1].length > 0)
        copy_field(gps_data.time, sizeof(gps_data.time), &msg->fields[1]);

    /* Field 2: Latitude */
    if (msg->num_fields > 2 && msg->fields[2].length > 0)
        copy_field(gps_data.latitude,
                   sizeof(gps_data.latitude),
                   &msg->fields[2]);

    /* Field 3: Latitude direction */
    if (msg->num_fields > 3 && msg->fields[3].length > 0)
        copy_field(gps_data.lat_dir, sizeof(gps_data.lat_dir), &msg->fields[3]);

    /* Field 4: Longitude */
    if (msg->num_fields > 4 && msg->fields[4].length > 0)
        copy_field(gps_data.longitude,
                   sizeof(gps_data.longitude),
                   &msg->fields[4]);

    /* Field 5: Longitude direction */
    if (msg->num_fields > 5 && msg->fields[5].length > 0)
        copy_field(gps_data.lon_dir, sizeof(gps_data.lon_dir), &msg->fields[5]);

    /* Field 6: Fix quality */
    if (msg->num_fields > 6 && msg->fields[6].length > 0)
        gps_data.fix_quality = parse_field_int(&msg->fields[6]);
    else
        gps_data.fix_quality = 0;

    /* Field 7: Number of satellites */
    if (msg->num_fields > 7 && msg->fields[7].length > 0)
        gps_data.satellites = parse_field_int(&msg->fields[7]);
    else
        gps_data.satellites = 0;

    /* Field 9: Altitude */
    if (msg->num_fields > 9 && msg->fields[9].length > 0)
        copy_field(gps_data.altitude,
                   sizeof(gps_data.altitude),
                   &msg->fields[9]);

    gps_data.data_valid = (gps_data.fix_quality > 0);
}

/* Parse GPRMC sentence (recommended minimum data) */
static void parse_gprmc(const nmea_message_t* msg) {
    /* Field 7: Speed over ground (knots) */
    if (msg->num_fields > 7 && msg->fields[7].length > 0)
        copy_field(gps_data.speed, sizeof(gps_data.speed), &msg->fields[7]);

    /* Field 8: Course over ground */
    if (msg->num_fields > 8 && msg->fields[8].length > 0)
        copy_field(gps_data.course, sizeof(gps_data.course), &msg->fields[8]);

    /* Field 9: Date */
    if (msg->num_fields > 9 && msg->fields[9].length > 0)
        copy_field(gps_data.date, sizeof(gps_data.date), &msg->fields[9]);
}

/* Process complete NMEA sentence */
static void process_nmea_sentence(const char* sentence, size_t sentence_size) {
    printf("Processing: '");
    nmea_print_escaped(sentence, sentence_size);
    printf("'\n");

    nmea_message_t msg;
    if (!nmea_parse_message(&msg, sentence, sentence_size)) {
        fprintf(stderr, "Failed to parse NMEA message.\n");
        return;
    }

    if (msg.received_checksum != msg.calculated_checksum) {
        fprintf(stderr,
                "Checksum error: received 0x%02X, calculated 0x%02X.\n",
                msg.received_checksum,
                msg.calculated_checksum);
        return;
    }

    /* Check sentence type (field 0, without '$' delimiter) */
    if (msg.fields[0].length < 5) {
        fprintf(stderr, "Invalid sentence type.\n");
        return;
    }

    const char* type = msg.fields[0].data;
    if (strncmp(type, "GPGGA", 5) == 0 || strncmp(type, "GNGGA", 5) == 0) {
        parse_gpgga(&msg);
    } else if (strncmp(type, "GPRMC", 5) == 0 ||
               strncmp(type, "GNRMC", 5) == 0) {
        parse_gprmc(&msg);
    }
}

/* Read and process GPS data from ring buffer */
static void read_gps_data(void) {
    static char nmea_buffer[NMEA_BUFFER_SIZE];
    static int nmea_index = 0;

    int c;
    while ((c = rx_getc()) >= 0) {
        /* Start of new sentence */
        if (c == '$')
            nmea_index = 0;

        /* Accumulate sentence data */
        if (nmea_index < NMEA_BUFFER_SIZE)
            nmea_buffer[nmea_index++] = (char)c;

        /* End of sentence */
        if (c == '\n' && nmea_index > 1) {
            /* Only process if it looks like a valid NMEA sentence */
            if (nmea_buffer[0] == '$')
                process_nmea_sentence(nmea_buffer, nmea_index);
            nmea_index = 0;
        }
    }
}

/* Convert NMEA coordinate (DDMM.MMMM or DDDMM.MMMM) to decimal degrees */
static double nmea_coord_to_decimal(const char* coord, int deg_digits) {
    double raw     = atof(coord);
    int degrees    = (int)(raw / 100);
    double minutes = raw - (degrees * 100);
    return degrees + (minutes / 60.0);
}

/* Display parsed GPS data */
static void display_gps_info(void) {
    printf("\n========== GPS STATUS ==========\n");
    printf("Fix Quality: %d\n", gps_data.fix_quality);
    printf("Satellites: %d\n", gps_data.satellites);
    printf("Valid Data: %s\n", gps_data.data_valid ? "YES" : "NO");

    if (gps_data.data_valid) {
        printf("---------------------------------\n");

        /* Format datetime: NMEA time is HHMMSS.ss, date is DDMMYY */
        if (strlen(gps_data.time) >= 6 && strlen(gps_data.date) >= 6) {
            printf("DateTime: 20%.2s.%.2s.%.2s %.2s:%.2s:%.2s\n",
                   &gps_data.date[4],
                   &gps_data.date[2],
                   &gps_data.date[0],
                   &gps_data.time[0],
                   &gps_data.time[2],
                   &gps_data.time[4]);
        } else {
            printf("Time: %s\n", gps_data.time);
            printf("Date: %s\n", gps_data.date);
        }

        /* Convert NMEA coords (DDMM.MMMM) to decimal degrees */
        double lat = nmea_coord_to_decimal(gps_data.latitude, 2);
        double lon = nmea_coord_to_decimal(gps_data.longitude, 3);
        printf("Latitude: %.6f %s\n", lat, gps_data.lat_dir);
        printf("Longitude: %.6f %s\n", lon, gps_data.lon_dir);
        printf("Altitude: %s m\n", gps_data.altitude);

        /* Convert speed from knots to km/h (1 knot = 1.852 km/h) */
        double speed_knots = atof(gps_data.speed);
        printf("Speed: %.2f km/h\n", speed_knots * 1.852);

        printf("Course: %s degrees\n", gps_data.course);
    }
    printf("================================\n\n");
}

int main(void) {
    /*
     * Initialize standard I/O for USB serial output.
     */
    stdio_init_all();

    /*
     * Wait for USB connection.
     */
    sleep_ms(2000);
    printf("\n\n=================================\n");
    printf("RP2350 GY-GPS6MV2 GPS Test\n");
    printf("=================================\n\n");

    /*
     * Initialize UART for GPS module.
     */
    uart_init(GPS_UART_ID, GPS_BAUD_RATE);
    gpio_set_function(GPS_UART_TX_PIN, GPIO_FUNC_UART);
    gpio_set_function(GPS_UART_RX_PIN, GPIO_FUNC_UART);

    /* Configure UART parameters: 8N1 (8 data bits, no parity, 1 stop bit) */
    uart_set_format(GPS_UART_ID, 8, 1, UART_PARITY_NONE);

    /* Set up UART RX interrupt */
    int uart_irq = (GPS_UART_ID == uart0) ? UART0_IRQ : UART1_IRQ;
    irq_set_exclusive_handler(uart_irq, on_uart_rx);
    irq_set_enabled(uart_irq, true);
    uart_set_irq_enables(GPS_UART_ID, true, false);

    printf("UART initialized: %d baud\n", GPS_BAUD_RATE);
    printf("GPS RX pin: GP%d\n", GPS_UART_RX_PIN);
    printf("GPS TX pin: GP%d (not used)\n", GPS_UART_TX_PIN);
    printf("\nWaiting for GPS data...\n\n");

    /*
     * Initialize the LED GPIO for status indication.
     */
    gpio_init(PICO_DEFAULT_LED_PIN);
    gpio_set_dir(PICO_DEFAULT_LED_PIN, GPIO_OUT);

    uint32_t last_display_time = 0;
    const uint32_t display_interval =
      10000; /* Display summary every 10 seconds */

    /*
     * Main loop: Read GPS data and display information.
     */
    for (;;) {
        /* Read and process GPS data */
        read_gps_data();

        /* Toggle LED to show activity */
        gpio_put(PICO_DEFAULT_LED_PIN, 1);
        sleep_ms(50);
        gpio_put(PICO_DEFAULT_LED_PIN, 0);
        sleep_ms(50);

        /* Display GPS summary periodically */
        uint32_t now = to_ms_since_boot(get_absolute_time());
        if (now - last_display_time >= display_interval) {
            display_gps_info();
            last_display_time = now;
        }
    }

    return 0;
}
