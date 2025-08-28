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

#include <errno.h>
#include <stdio.h>
#include <string.h> /* strerror */

/*
 * Return the size of a file using 'fseek' and 'ftell'.
 */
static size_t get_file_size(FILE* fp) {
    /* Preserve current position in the file */
    const long old_position = ftell(fp);
    if (old_position < 0) {
        fprintf(stderr, "Failed to get file position: %s\n", strerror(errno));
    }

    /* Move to the end of the file, and obtain the size */
    if (fseek(fp, 0L, SEEK_END) < 0)
        fprintf(stderr,
                "Could not move to the end of file: %s\n",
                strerror(errno));
    const long file_sz = ftell(fp);
    if (file_sz < 0)
        fprintf(stderr, "Failed to get file position: %s\n", strerror(errno));

    /* Restore the old position and return the total size in bytes */
    fseek(fp, old_position, SEEK_SET);
    return file_sz;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s FILENAME\n", argv[0]);
        return 1;
    }

    const char* filename = argv[1];
    FILE* fp             = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr,
                "Failed to open file '%s': %s\n",
                filename,
                strerror(errno));
        return 1;
    }

    const size_t file_size = get_file_size(fp);
    printf("Size of '%s': %zu\n", filename, file_size);

    return 0;
}
