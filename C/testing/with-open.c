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
 * ----------------------------------------------------------------------------
 *
 * This program shows an example of a 'WITH_OPEN' macro. It lets the user
 * declare a block in which the specified file is open in the specified mode.
 *
 * Compile this program with:
 *
 *     $ gcc -Wall -Wextra -Wpedantic -o with-open.out with-open.c
 *
 * And test it with:
 *
 *     $ ./with-open.out
 *     $ cat /tmp/foo.txt
 *     Hello, world!
 *
 * This program is valid ISO C90, so you can compile it with the '-ansi' option.
 */

#include <assert.h>
#include <stdio.h>

/*
 * Open the specified PATHNAME with the specified MODE using 'fopen', and save
 * its result in a specified 'FILE*' variable.
 *
 * A call to this macro should be followed by a statement or block:
 *
 *     WITH_OPEN(fp, path, "r") {
 *         ...
 *     }
 *
 * Inside that block, the variable specified as the first argument of this macro
 * (FP_VAR) is guaranteed to be a valid 'FILE*' that can be used in calls like
 * 'fread' or 'fwrite'.
 *
 * After the statement or block is executed once, the 'FILE*' variable is set to
 * NULL.
 *
 * Caveats:
 *
 * - If the call to 'fopen' fails, the body won't be entered, even a single
 *   time.
 */
#define WITH_OPEN(FP_VAR, PATHNAME, MODE)                                      \
    for (FP_VAR = fopen(PATHNAME, MODE); FP_VAR != NULL;                       \
         fclose(FP_VAR), FP_VAR = NULL)

/*
 * Example program using the 'WITH_OPEN' macro.
 */
int main(void) {
    FILE* fp;
    WITH_OPEN(fp, "/tmp/foo.txt", "w") {
        assert(fp != NULL);
        fprintf(fp, "Hello, world!\n");
    }

    assert(fp == NULL);
    return 0;
}
