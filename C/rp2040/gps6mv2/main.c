
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "pico/stdlib.h"
#include "hardware/uart.h"
#include "hardware/irq.h"

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
        char c = uart_getc(GPS_UART_ID);
        uint16_t next_head = (rx_head + 1) & (RX_BUFFER_SIZE - 1);
        if (next_head != rx_tail) {
            rx_buffer[rx_head] = c;
            rx_head = next_head;
        }
        /* If buffer full, drop the character */
    }
}

/* Read a character from the RX ring buffer, returns -1 if empty */
static int rx_getc(void) {
    if (rx_head == rx_tail)
        return -1;
    char c = rx_buffer[rx_tail];
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

static void print_escaped(const char* buffer, size_t buffer_len) {
    for (size_t i = 0; i < buffer_len; i++) {
        const char c = buffer[i];
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

/* Calculate NMEA checksum */
static uint8_t calculate_checksum(const char* sentence, size_t length) {
    assert(sentence != NULL && length > 0);

    if (sentence[0] != '$')
        return 0;

    /*
     * The NMEA checksum is the bit-wise XOR of all characters between the
     * starting '$' and the checksum separator '*'.
     */
    uint8_t checksum = 0;
    for (int i = 1; i < length && sentence[i] != '*'; i++)
        checksum ^= sentence[i];
    return checksum;
}

/* Verify NMEA checksum */
static bool verify_checksum(const char* sentence) {
    const char* checksum_pos = strchr(sentence, '*');
    if (checksum_pos == NULL) {
        fprintf(stderr, "Could not find checksum marker in sentence.\n");
        return false;
    }

    /* FIXME: Check dangerous calls */
    int length         = checksum_pos - sentence;
    uint8_t calculated = calculate_checksum(sentence, length);
    uint8_t received   = (uint8_t)strtol(checksum_pos + 1, NULL, 16);

    const bool result = (calculated == received);
    if (!result)
        fprintf(stderr,
                "Calculated checksum (%d) and received checksum (%d) do not "
                "match.\n",
                calculated,
                received);

    return result;
}

/*
 * Get pointer to field N (0-indexed) in a comma-separated sentence.
 * Returns pointer to field start and sets *len to field length.
 * Returns NULL if field not found.
 */
static const char* get_field(const char* sentence, size_t sentence_size,
                             int field_num, size_t* len) {
    const char* pos = sentence;
    const char* end = sentence + sentence_size;
    int current_field = 0;

    /* Skip to the requested field */
    while (pos < end && current_field < field_num) {
        if (*pos == ',')
            current_field++;
        pos++;
    }

    if (pos >= end)
        return NULL;

    /* Find end of this field */
    const char* field_start = pos;
    while (pos < end && *pos != ',' && *pos != '*' && *pos != '\r' && *pos != '\n')
        pos++;

    *len = pos - field_start;
    return field_start;
}

/* Parse GPGGA sentence (position and fix data) */
static void parse_gpgga(const char* sentence, size_t sentence_size) {
    size_t len;
    const char* field;

    /* Field 1: UTC time */
    if ((field = get_field(sentence, sentence_size, 1, &len)) && len > 0)
        sscanf(field, "%11[^,*\r\n]", gps_data.time);

    /* Field 2: Latitude */
    if ((field = get_field(sentence, sentence_size, 2, &len)) && len > 0)
        sscanf(field, "%15[^,*\r\n]", gps_data.latitude);

    /* Field 3: Latitude direction */
    if ((field = get_field(sentence, sentence_size, 3, &len)) && len > 0)
        sscanf(field, "%1[^,*\r\n]", gps_data.lat_dir);

    /* Field 4: Longitude */
    if ((field = get_field(sentence, sentence_size, 4, &len)) && len > 0)
        sscanf(field, "%15[^,*\r\n]", gps_data.longitude);

    /* Field 5: Longitude direction */
    if ((field = get_field(sentence, sentence_size, 5, &len)) && len > 0)
        sscanf(field, "%1[^,*\r\n]", gps_data.lon_dir);

    /* Field 6: Fix quality */
    if ((field = get_field(sentence, sentence_size, 6, &len)) && len > 0)
        sscanf(field, "%d", &gps_data.fix_quality);
    else
        gps_data.fix_quality = 0;

    /* Field 7: Number of satellites */
    if ((field = get_field(sentence, sentence_size, 7, &len)) && len > 0)
        sscanf(field, "%d", &gps_data.satellites);
    else
        gps_data.satellites = 0;

    /* Field 9: Altitude */
    if ((field = get_field(sentence, sentence_size, 9, &len)) && len > 0)
        sscanf(field, "%11[^,*\r\n]", gps_data.altitude);

    gps_data.data_valid = (gps_data.fix_quality > 0);
}

/* Parse GPRMC sentence (recommended minimum data) */
static void parse_gprmc(const char* sentence, size_t sentence_size) {
    size_t len;
    const char* field;

    /* Field 7: Speed over ground (knots) */
    if ((field = get_field(sentence, sentence_size, 7, &len)) && len > 0)
        sscanf(field, "%11[^,*\r\n]", gps_data.speed);

    /* Field 8: Course over ground */
    if ((field = get_field(sentence, sentence_size, 8, &len)) && len > 0)
        sscanf(field, "%11[^,*\r\n]", gps_data.course);

    /* Field 9: Date */
    if ((field = get_field(sentence, sentence_size, 9, &len)) && len > 0)
        sscanf(field, "%11[^,*\r\n]", gps_data.date);
}

/* Process complete NMEA sentence */
static void process_nmea_sentence(const char* sentence, size_t sentence_size) {
    /* Print raw NMEA sentence */
    printf("Processing: '");
    print_escaped(sentence, sentence_size);
    printf("'\n");

    if (!verify_checksum(sentence)) {
        fprintf(stderr, "Checksum error.\n");
        return;
    }

    /* Parse specific sentence types */
    if (strncmp(sentence, "$GPGGA", 6) == 0 ||
        strncmp(sentence, "$GNGGA", 6) == 0) {
        parse_gpgga(sentence, sentence_size);
    } else if (strncmp(sentence, "$GPRMC", 6) == 0 ||
               strncmp(sentence, "$GNRMC", 6) == 0) {
        parse_gprmc(sentence, sentence_size);
    } else {
        fprintf(stderr, "Unknown sentence type. Not processing.\n");
        return;
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

/* Display parsed GPS data */
static void display_gps_info(void) {
    printf("\n========== GPS STATUS ==========\n");
    printf("Fix Quality: %d\n", gps_data.fix_quality);
    printf("Satellites: %d\n", gps_data.satellites);
    printf("Valid Data: %s\n", gps_data.data_valid ? "YES" : "NO");

    if (gps_data.data_valid) {
        printf("---------------------------------\n");
        printf("Time: %s\n", gps_data.time);
        printf("Date: %s\n", gps_data.date);
        printf("Latitude: %s %s\n", gps_data.latitude, gps_data.lat_dir);
        printf("Longitude: %s %s\n", gps_data.longitude, gps_data.lon_dir);
        printf("Altitude: %s m\n", gps_data.altitude);
        printf("Speed: %s knots\n", gps_data.speed);
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
