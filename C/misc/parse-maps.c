/*
 * Parsing /proc/self/maps to get the current module location and size in
 * memory.
 *
 * NOTE: This can be done in a simpler way with a good scanf() format. For a
 * good example, see https://github.com/8dcc/libsigscan
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static void get_module_bounds(uintptr_t* start, uintptr_t* end) {
    bool start_set = false;
    bool end_set   = false;
    *start         = 0;
    *end           = 0;

    FILE* fd = fopen("/proc/self/maps", "r");
    if (!fd)
        return;

    static char addr_buf[] = "FFFFFFFFFFFFFFFF";
    int addr_buf_pos;

    int c;
    while ((c = fgetc(fd)) != EOF) {
        /* Read first address of the line */
        addr_buf_pos = 0;
        do {
            addr_buf[addr_buf_pos++] = c;
        } while ((c = fgetc(fd)) != '-');
        addr_buf[addr_buf_pos] = '\0';

        uint64_t start_addr = strtoull(addr_buf, NULL, 16);

        /* Read second address of the line */
        addr_buf_pos = 0;
        while ((c = fgetc(fd)) != ' ')
            addr_buf[addr_buf_pos++] = c;
        addr_buf[addr_buf_pos] = '\0';

        uint64_t end_addr = strtoull(addr_buf, NULL, 16);

        /* "rwxp" */
        c = fgetc(fd);

        /* We can read it, otherwise ignore */
        if (c == 'r') {
            if (*start > start_addr || !start_set) {
                *start    = start_addr;
                start_set = true;
            }

            if (*end < end_addr || !end_set) {
                *end    = end_addr;
                end_set = true;
            }
        }

        /* Skip until end of line or end of file */
        while ((c = fgetc(fd)) != '\n' && c != EOF)
            ;
    }

    fclose(fd);
}

static void print_maps(void) {
    FILE* fd = fopen("/proc/self/maps", "r");
    int c;
    while ((c = fgetc(fd)) != EOF)
        putchar(c);
    fclose(fd);
}

int main(void) {
    uintptr_t start, end;
    get_module_bounds(&start, &end);

    printf("File output:\n");
    print_maps();

    printf("\nFunction output:\n"
           "Start: 0x%lX\n"
           "End: 0x%lX\n",
           start, end);

    return 0;
}
