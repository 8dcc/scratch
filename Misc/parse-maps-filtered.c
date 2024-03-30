/*
 * Parse /proc/self/maps to get the start and end addresses of the specified
 * module.
 *
 * The function assumes the format of maps is always:
 * 0000DEADBEEF-0000ABADCAFE rwxp 000123AB 103:03 12345678   /path/module
 *
 * NOTE: You can replace most calls to isspace() and remove the <ctype.h>
 * include by just checking if c is a space.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h> /* strtoull() */
#include <ctype.h>  /* isspace() */

static void get_module_bounds(const char* module_name, uintptr_t* start,
                              uintptr_t* end) {
    bool start_set = false;
    bool end_set   = false;
    *start         = 0;
    *end           = 0;

    FILE* fd = fopen("/proc/self/maps", "r");
    if (!fd)
        return;

    /* For converting to uint64_t using strtoull() */
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

        /* Parse "rwxp". For now we only care about read permissions. */
        bool is_readable = ((c = fgetc(fd)) == 'r');

        bool name_matches = true;
        if (module_name == NULL) {
            /* We don't want to filter the module name, skip until end of line
             * or end of file. */
            while ((c = fgetc(fd)) != '\n' && c != EOF)
                ;
        } else {
            /* Skip permissions and single space */
            while (!isspace(c = fgetc(fd)))
                ;

            /* Skip 3rd column and single space */
            while (!isspace(c = fgetc(fd)))
                ;

            /* Skip 4th column and single space */
            while (!isspace(c = fgetc(fd)))
                ;

            /* Skip 5th column */
            while (!isspace(c = fgetc(fd)))
                ;

            /* Skip spacing until the module name. First char of module name
             * will be saved in `c' after this loop. */
            while (isspace(c = fgetc(fd)))
                ;

            /* Compare module name. Note that the output of maps has absolute
             * paths. */
            int i = 0;
            do {
                /* A character did not match the name of the module in this
                 * line, ignore it. We can't break out of the `for' because we
                 * have to get to the newline anyway. */

                if (c != module_name[i])
                    name_matches = false;
                else if (module_name[i] != '\0')
                    i++;
            } while ((c = fgetc(fd)) != '\n');
        }

        /* We can read it, and it's the module we are looking for, check if the
         * addresses are smaller/bigger. Otherwise ignore. */
        if (is_readable && name_matches) {
            if (*start > start_addr || !start_set) {
                *start    = start_addr;
                start_set = true;
            }

            if (*end < end_addr || !end_set) {
                *end    = end_addr;
                end_set = true;
            }
        }
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
    /* Use NULL to disable name filtering */
    static const char* MODULE_NAME = "/usr/lib/libc.so.6";

    uintptr_t start, end;
    get_module_bounds(MODULE_NAME, &start, &end);

    printf("File output:\n");
    print_maps();

    printf("\nFunction output:\n"
           "Start: 0x%lX\n"
           "End: 0x%lX\n",
           start, end);

    return 0;
}
