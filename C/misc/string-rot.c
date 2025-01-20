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
 * ============================================================================
 *
 * Rotate a string with the specified offset. When the 'OFFSET' argument is
 * positive, the 'INPUT' bytes are rotated right; and if the 'OFFSET' argument
 * is negative, the 'INPUT' bytes are rotated left.
 *
 * The behavior is similar to the 'ROR' (for positive values) and 'ROL' (for
 * negative values) x86 instructions, except with ASCII values, rather than
 * bits.
 *
 * The output of the following command should print the INPUT with no changes.
 *
 *     echo INPUT | ./string-rot.out <N> | ./string-rot.out <-N>
 *
 * For example:
 *
 *     echo "Hello, world!" | ./string-rot.out 5 | ./string-rot.out -5
 */

#include <limits.h>
#include <stdio.h>

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s OFFSET < INPUT.txt\n", argv[0]);
        return 1;
    }

    int offset;
    if (sscanf(argv[1], "%d", &offset) != 1) {
        fprintf(stderr, "Error: Invalid offset format.\n");
        return 1;
    }

    /*
     * Make sure we don't rotate past the original index. Overflows and
     * underflows are handled below.
     */
    offset %= UCHAR_MAX;

    int c;
    while ((c = getchar()) != EOF) {
        c += offset;

        /*
         * Handle undeflows and overflows.
         */
        if (c < 0)
            c += UCHAR_MAX + 1;
        else if (c > UCHAR_MAX)
            c -= UCHAR_MAX - 1;

        putchar(c);
    }

    return 0;
}
