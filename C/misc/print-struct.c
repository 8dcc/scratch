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
 * This program prints the layout of a C structure in different formats.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LENGTH(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

#define PRINT_MEMBER_BYTES(INSTANCE, MEMBER)                                   \
    do {                                                                       \
        memset(&INSTANCE, 0, sizeof(INSTANCE));                                \
        INSTANCE.MEMBER = -1;                                                  \
        char arr[sizeof(INSTANCE)];                                            \
        memset(arr, 0, sizeof(arr));                                           \
        memcpy(arr, &INSTANCE, sizeof(INSTANCE));                              \
        for (size_t i = 0; i < LENGTH(arr); i++)                               \
            for (int j = 7; j >= 0; j--)                                       \
                putchar((arr[i] & (1 << j)) ? '#' : '.');                      \
        printf(" (%s)\n", #MEMBER);                                            \
    } while (0)

#define PRINT_MEMBER_INTEGERS(INSTANCE, MEMBER)                                \
    do {                                                                       \
        memset(&INSTANCE, 0, sizeof(INSTANCE));                                \
        INSTANCE.MEMBER = -1;                                                  \
        int arr[sizeof(INSTANCE) / sizeof(int)];                               \
        memset(arr, 0, sizeof(arr));                                           \
        memcpy(arr, &INSTANCE, sizeof(INSTANCE));                              \
        for (size_t i = 0; i < LENGTH(arr); i++)                               \
            for (int j = 3; j >= 0; j--)                                       \
                for (int k = 7; k >= 0; k--)                                   \
                    putchar((arr[i] & (1 << (j * 8 + k))) ? '#' : '.');        \
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

    printf("Printing as %zu-bit bytes:\n", sizeof(char));
    PRINT_MEMBER_BYTES(s, member1);
    PRINT_MEMBER_BYTES(s, member2);
    PRINT_MEMBER_BYTES(s, member3);
    PRINT_MEMBER_BYTES(s, member4);
    PRINT_MEMBER_BYTES(s, member5);
    PRINT_BYTE_MARKS(s);

    printf("\nPrinting as %zu-byte integers (reversing endianness):\n",
           sizeof(int));
    PRINT_MEMBER_INTEGERS(s, member1);
    PRINT_MEMBER_INTEGERS(s, member2);
    PRINT_MEMBER_INTEGERS(s, member3);
    PRINT_MEMBER_INTEGERS(s, member4);
    PRINT_MEMBER_INTEGERS(s, member5);
    PRINT_BYTE_MARKS(s);

    printf("\nTotal size: %zu bytes.\n", sizeof(MyStruct));

    return 0;
}
