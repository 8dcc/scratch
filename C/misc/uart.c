/*
 * Copyright 2024 8dcc
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
 * Simple program for writing data through UART.
 *
 * See also:
 *   https://man.cx/open(2)
 *   https://man.cx/termios(3)
 *   https://pubs.opengroup.org/onlinepubs/007908799/xsh/termios.h.html
 */

#include <stdbool.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <unistd.h>  /* read(), write() */
#include <fcntl.h>   /* open() */
#include <termios.h> /* tcgetattr(), etc. */

/*
 * Initializes an UART file descriptor, and returns it.
 */
static int uart_init(const char* device_path) {
    /*
     * Flags from <fcntl.h>, see open(2):
     *
     *   - O_RDWR: Read and write privileges.
     *   - O_NOCTTY: Since the file is a terminal device, we set this to avoid
     *     it becoming the process's controlling terminal.
     */
    int fd = open(device_path, O_RDWR | O_NOCTTY);
    if (fd == -1)
        return -1;

    /*
     * Configure the UART.
     *
     * Flags from <termios.h>, see termios(3):
     *
     *   - B<RATE>: Baud rate, where <RATE> might be 9600, 57600, 115200, etc.
     *   - CS<NUM>: Character size mask. Values are CS5, CS6, CS7, or CS8.
     *   - CREAD: Enable the receiver.
     *   - CLOCAL: Ignore modem control lines.
     *
     * We don't set `PARENB' to avoid the parity bit, and we don't set `CSTOPB'
     * because we only need 1 stop bit.
     *
     * Other constants:
     *   - TCIFLUSH: Flushes data received but not read.
     *   - TCSANOW: The change occurs immediately (instead of waiting for a
     *     flush).
     */
    struct termios options;
    if (tcgetattr(fd, &options) < 0) {
        close(fd);
        return -1;
    }

    options.c_cflag     = B9600 | CS8 | CREAD | CLOCAL;
    options.c_oflag     = 0;
    options.c_lflag     = 0;
    options.c_cc[VMIN]  = 1;
    options.c_cc[VTIME] = 1;
    tcflush(fd, TCIFLUSH);

    if (tcsetattr(fd, TCSANOW, &options) < 0) {
        close(fd);
        return -1;
    }

    return fd;
}

static inline bool uart_write_str(int fd, const char* str) {
    return write(fd, str, strlen(str)) >= 0;
}

static inline bool uart_read_line(int fd, char* str, size_t str_sz) {
    size_t bytes_left    = str_sz - 1;
    size_t bytes_written = 0;

    while (bytes_left > 0) {
        const int num_read = read(fd, &str[bytes_written], bytes_left);
        if (num_read < 0)
            return false;
        if (num_read == 0)
            continue;

        bytes_written += num_read;
        bytes_left -= num_read;

        if (str[bytes_written - 1] == '\n')
            break;
    }

    str[bytes_written] = '\0';
    return true;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s UART-DEV-PATH\n", argv[0]);
        return 1;
    }

    const char* uart_dev = argv[1];
    const int uart_fd    = uart_init(uart_dev);
    if (uart_fd < 0) {
        fprintf(stderr, "Could not initialize UART device '%s': %s\n", uart_dev,
                strerror(errno));
        return 1;
    }

    /*
     * Write some test string.
     */
    printf("Writing some test string...\n");
    if (!uart_write_str(uart_fd, "*ping*\n")) {
        fprintf(stderr, "Failed to write to UART device: %s\n",
                strerror(errno));
        close(uart_fd);
        return 1;
    }

    /*
     * Read some string, and write it back with a different case.
     */
    char line[256];
    if (!uart_read_line(uart_fd, line, sizeof(line))) {
        fprintf(stderr, "Failed to read from UART device.\n");
        close(uart_fd);
        return 1;
    }
    printf("Original string: '%s'\n", line);

    for (size_t i = 0; line[i] != '\0'; i++) {
        if (isupper(line[i]))
            line[i] = tolower(line[i]);
        else if (islower(line[i]))
            line[i] = toupper(line[i]);
    }
    printf("New string: '%s'\n", line);

    if (!uart_write_str(uart_fd, line)) {
        fprintf(stderr, "Failed to write to UART device: %s\n",
                strerror(errno));
        close(uart_fd);
        return 1;
    }

    close(uart_fd);
    return 0;
}
