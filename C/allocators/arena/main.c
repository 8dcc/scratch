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

#include "arena.h"

typedef struct StrList {
    char* str;
    struct StrList* next;
} StrList;

#define MAX_LINE_SZ 80

StrList* read_file_lines(Arena arena, FILE* fp) {
    /* Stack buffer for reading a single line */
    static char cur_line[MAX_LINE_SZ];
    size_t cur_line_pos = 0;

    StrList first_line;
    first_line.next = NULL;
    StrList* line   = &first_line;

    int c = fgetc(fp);
    for (;;) {
        if (c == '\n' || c == EOF) {
            /* Allocate new `StrList' structure */
            line->next = arena_alloc(&arena, sizeof(StrList));
            line       = line->next;
            if (line == NULL)
                return NULL;

            /* Initialize it */
            line->next = NULL;
            line->str  = arena_alloc(&arena, cur_line_pos + 1);
            if (line->str == NULL)
                return NULL;

            /* Copy the actual line contents to the linked list */
            memcpy(line->str, cur_line, cur_line_pos);
            line->str[cur_line_pos] = '\0';

            /* Reset the line position for the next line */
            cur_line_pos = 0;

            /* If we got EOF, stop. Otherwise, continue */
            if (c == EOF)
                break;

            c = fgetc(fp);
            continue;
        }

        /* Make sure we don't overflow the line buffer */
        if (cur_line_pos >= MAX_LINE_SZ) {
            fprintf(stderr, "Line is too long, terminating early.");
            while (c != '\n' && c != EOF)
                c = fgetc(fp);
            continue;
        }

        /* Append to current line */
        cur_line[cur_line_pos++] = c;

        /* Get next character from the file */
        c = fgetc(fp);
    }

    return first_line.next;
}

void print_file_lines(Arena arena, FILE* fp) {
    for (int i = 0; i < 80; i++)
        putchar('-');
    putchar('\n');

    StrList* line = read_file_lines(arena, fp);
    for (; line != NULL; line = line->next)
        printf("%s\n", line->str);

    for (int i = 0; i < 80; i++)
        putchar('-');
    putchar('\n');
}

int main(void) {
    /*
     * Create a new arena, with the specified size. It will be used for reading
     * the lines of a file.
     */
    Arena arena = arena_new(0x1000);

    /*
     * Open some example file for reading data into the arena.
     */
    const char* target_file = "/etc/profile";
    printf("main: Reading '%s', and printing lines.\n", target_file);
    FILE* fp = fopen(target_file, "r");
    assert(fp != NULL);
    print_file_lines(arena, fp);
    fclose(fp);

    /*
     * Make sure `arena_alloc_aligned' works fine.
     */
    const size_t alignment = 16;
    for (int i = 1; i <= 35; i++) {
        void* ptr = arena_alloc_aligned(&arena, i, alignment);
        assert(((uintptr_t)ptr % alignment) == 0);
    }
    printf("main: All pointers aligned correctly.\n");

    /*
     * Destroy the arena we allocated. It becomes unusable.
     */
    arena_destroy(arena);

    return 0;
}
