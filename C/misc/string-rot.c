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

    int c;
    while ((c = getchar()) != EOF)
        putchar((c + offset) % CHAR_MAX);

    return 0;
}
