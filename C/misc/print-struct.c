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
 *
 * ----------------------------------------------------------------------------
 *
 * This program prints the layout of a C structure.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define PRINT_MEMBER(INSTANCE, MEMBER)                                         \
    do {                                                                       \
        memset(&INSTANCE, 0, sizeof(INSTANCE));                                \
        INSTANCE.MEMBER = -1;                                                  \
        char arr[sizeof(INSTANCE)];                                            \
        memset(arr, 0, sizeof(arr));                                           \
        memcpy(arr, &INSTANCE, sizeof(INSTANCE));                              \
        for (size_t i = 0; i < sizeof(arr); i++)                               \
            for (int j = 7; j >= 0; j--)                                       \
                putchar((arr[i] & (1 << j)) ? '#' : '.');                      \
        printf(" (%s)\n", #MEMBER);                                            \
    } while (0)

#define PRINT_BYTE_MARKS(INSTANCE)                                             \
    do {                                                                       \
        for (size_t i = 0; i < sizeof(INSTANCE); i++)                          \
            printf("^%-3zX    ", i);                                           \
        putchar('\n');                                                         \
    } while (0)

/*----------------------------------------------------------------------------*/

typedef struct {
    int member1;
    int member2 : 6;
    int member3 : 12;
    long member4;
    char member5 : 1;
} MyStruct;

int main(void) {
    MyStruct s;

    PRINT_MEMBER(s, member1);
    PRINT_MEMBER(s, member2);
    PRINT_MEMBER(s, member3);
    PRINT_MEMBER(s, member4);
    PRINT_MEMBER(s, member5);
    PRINT_BYTE_MARKS(s);

    printf("Total size: %zu bytes.\n", sizeof(MyStruct));

    return 0;
}
