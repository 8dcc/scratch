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
 */

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "arena_twosided.h"

void print_twosided_arena(Arena a) {
    const size_t real_off_back = a.size - a.off_back;
    size_t i;

    putchar('|');
    for (i = 0; i < a.off_front; i++)
        putchar('.');
    for (; i < real_off_back; i++)
        putchar(' ');
    for (; i < a.size; i++)
        putchar('.');
    puts("|");

    putchar(' ');
    for (i = 0; i < a.off_front; i++)
        putchar(' ');
    putchar('^');
    for (; i < real_off_back - 1; i++)
        putchar(' ');
    puts("^");
}

int main(void) {
    /*
     * The arena size should be smaller than your terminal width for
     * `print_twosided_arena'.
     */
    const size_t sz = 200;
    Arena arena     = arena_new(sz);

    puts("main: Printing initial state:");
    print_twosided_arena(arena);

    puts("main: Allocating some bytes from the front...");
    arena_alloc_front(&arena, sz / 3);

    puts("main: Allocating some bytes from the back...");
    arena_alloc_back(&arena, sz / 4);

    puts("main: Printing arena:");
    print_twosided_arena(arena);

    puts("main: Trying to allocate more bytes from the front, "
         "until overflow...");
    while (arena_alloc_front(&arena, 1) != NULL)
        ;

    puts("main: Got NULL with the following state:");
    print_twosided_arena(arena);

    arena_destroy(arena);
    return 0;
}
