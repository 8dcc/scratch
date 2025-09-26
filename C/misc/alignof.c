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
 * This program implements alternatives to the 'alignof' and 'alignas'
 * specifiers from C23. These should work in C99, C11 and C23.
 *
 * The 'alignof' operator is used to retraive the alignment requirement of a
 * type in bytes; meaning that all instances of that type must be in a memory
 * address divisible by that alignment.
 *
 * The 'alignas' operator specifies the alignment for a variable or structure
 * member. It receives the alignment value in bytes, and ensures that the memory
 * address of that variable will be a multiple of the specified value.
 */

#include <stdio.h>

/*----------------------------------------------------------------------------*/

#if __STDC_VERSION__ >= 201112L
/*
 * The '_Alignof' operator was added in C11. An 'alignof' macro was also
 * introduced in the 'stdalign.h' header, which was later promoted to an
 * operator in C23; we don't need to use it in either standard.
 */
#define ALIGNOF(TYPE) (_Alignof(TYPE))
#else               /* __STDC_VERSION__ < 201112L */
/*
 * In standards before C11, create an anonymous structure that is passed as the
 * first argument of 'offsetof'. Since structure members are aligned, and since
 * 'sizeof(char)' is 1, the offset of the 'x' member should match its alignment.
 */
#include <stddef.h> /* offsetof (since C89) */
#define ALIGNOF(TYPE)                                                          \
    (offsetof(                                                                 \
      struct {                                                                 \
          char c;                                                              \
          TYPE x;                                                              \
      },                                                                       \
      x))
#endif /* __STDC_VERSION__ < 201112L */

/*----------------------------------------------------------------------------*/

#if __STDC_VERSION__ >= 201112L
/*
 * Just like the '_Alignof' operator, the '_Alignas' operator was added in C11.
 */
#define ALIGNAS(SIZE) (_Alignas(SIZE))
#else /* __STDC_VERSION__ < 201112L */
#if defined(__GNUC__) || defined(__clang__)
#define ALIGNAS(SIZE) __attribute__((aligned(SIZE)))
#else /* !defined(__GNUC__) && !defined(__clang__) */
#error "This compiler does not have an 'alignas' equivalent."
#endif /* !defined(__GNUC__) && !defined(__clang__) */
#endif /* __STDC_VERSION__ < 201112L */

/*----------------------------------------------------------------------------*/

typedef struct {
    char c1;
    char c2;
    int n1;
    int n2;
    char c3;
    char c4;
    float f1;
    char c5;
} DummyStruct1;

typedef struct {
    char c1;
    ALIGNAS(4) char c2;
    char c3;
    ALIGNAS(sizeof(int)) char c4;
} DummyStruct2;

#define DUMP_TYPE(TYPE)                                                        \
    do {                                                                       \
        printf("| %-9s | %4zu | %9zu |\n",                                     \
               #TYPE,                                                          \
               sizeof(TYPE),                                                   \
               ALIGNOF(TYPE));                                                 \
    } while (0)

#define DUMP_MEMBER(STRUCTURE, MEMBER)                                         \
    do {                                                                       \
        STRUCTURE dummy;                                                       \
        const size_t size   = sizeof(dummy.MEMBER);                            \
        const size_t offset = offsetof(STRUCTURE, MEMBER);                     \
        printf("| %-15s | %4zu | %6zu | %9zu |\n",                             \
               #STRUCTURE "." #MEMBER,                                         \
               size,                                                           \
               offset,                                                         \
               offset + size - 1);                                             \
    } while (0)

int main(void) {
    printf("+------------------------------+\n");
    printf("| Type      | Size | Alignment |\n");
    printf("|-----------+------+-----------|\n");
    DUMP_TYPE(char);
    DUMP_TYPE(int);
    DUMP_TYPE(long);
    DUMP_TYPE(long long);
    DUMP_TYPE(float);
    DUMP_TYPE(double);
    printf("+------------------------------+\n\n");

    printf("+---------------------------------------------+\n");
    printf("| Member          | Size | Offset | Last used |\n");
    printf("|-----------------+------+--------+-----------|\n");
    DUMP_MEMBER(DummyStruct1, c1);
    DUMP_MEMBER(DummyStruct1, c2);
    DUMP_MEMBER(DummyStruct1, n1);
    DUMP_MEMBER(DummyStruct1, n2);
    DUMP_MEMBER(DummyStruct1, c3);
    DUMP_MEMBER(DummyStruct1, c4);
    DUMP_MEMBER(DummyStruct1, f1);
    DUMP_MEMBER(DummyStruct1, c5);
    printf("|-----------------+------+--------+-----------|\n");
    DUMP_MEMBER(DummyStruct2, c1);
    DUMP_MEMBER(DummyStruct2, c2);
    DUMP_MEMBER(DummyStruct2, c3);
    DUMP_MEMBER(DummyStruct2, c4);
    printf("+---------------------------------------------+\n");

    return 0;
}
