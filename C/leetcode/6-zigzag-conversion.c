
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * https://leetcode.com/problems/zigzag-conversion/
 *
 * 6. Zigzag Conversion
 * ====================
 *
 * The string "PAYPALISHIRING" is written in a zigzag pattern on a given number
 * of rows like this:
 *
 *     P   A   H   N
 *     A P L S I I G
 *     Y   I   R
 *
 * And then read line by line: "PAHNAPLSIIGYIR".
 *
 * Write the code that will take a string and make this conversion given a
 * number of rows.
 *
 * Notes
 * -----
 *
 * If we wanted to represent the zigzag pattern in a 2D matrix, we can simplify
 * it like so:
 *
 *      Original (7x4)         Simplified (5x4)
 *
 *      0     6     C     >    0 6   C
 *      1   5 7   B D     >    1 5 7 B D
 *      2 4   8 A   E     >    2 4 8 A E
 *      3     9     F     >    3   9   F
 *
 * Both will result in the same string: "06C157BD248AE39F".
 *
 * I am sure there is a mathematical way of calculating the new index in the
 * string based on the old position and the number of rows, specially after the
 * previous "simplified" version. However, I don't want to invest more time on
 * this.
 */

#define MAX(A, B) (((A) > (B)) ? (A) : (B))
#define CEIL(N)   ((N - (int)(N)) > 0 ? (int)(N + 1) : (int)(N))

char* convert(char* s, int numRows) {
    const size_t sz = strlen(s);

    if (numRows == 1) {
        char* result = malloc(sz + 1);
        strcpy(result, s);
        return result;
    }

    /* Number of columns in my simplified version. Each col (except the first
     * one) has (Height-1) digits. */
    const int numCols = CEIL((double)sz / MAX(numRows - 1, 1));

    /* Allocate the matrix for the zigzag */
    char* matrix = calloc(numRows * numCols, sizeof(char));

    /* First character of input is always the first character of the output */
    matrix[0] = s[0];

    /* Plot the input string into the matrix */
    size_t s_pos = 1;
    for (int x = 0; x < numCols; x++) {
        const bool odd_col = x & 1;

        /* Change direction every column */
        if (odd_col) {
            for (int y = numRows - 2; y >= 0 && s_pos < sz; y--)
                matrix[numCols * y + x] = s[s_pos++];
        } else {
            for (int y = 1; y < numRows && s_pos < sz; y++)
                matrix[numCols * y + x] = s[s_pos++];
        }
    }

    /* Convert the matrix into the result string */
    char* result   = malloc(sz + 1);
    int result_pos = 0;
    for (int y = 0; y < numRows; y++) {
        for (int x = 0; x < numCols; x++) {
            const char value = matrix[numCols * y + x];
            if (value != 0)
                result[result_pos++] = value;
        }
    }
    result[result_pos] = '\0';

    /* Return the resulting string */
    free(matrix);
    return result;
}

int main(void) {
    char case1[]  = "PAYPALISHIRING";
    char* result1 = convert(case1, 3);
    printf("Result 1: '%s'\n", result1);
    free(result1);

    char* result2 = convert(case1, 4);
    printf("Result 2: '%s'\n", result2);
    free(result2);

    char case3[]  = "A";
    char* result3 = convert(case3, 1);
    printf("Result 3: '%s'\n", result3);
    free(result3);

    char case4[]  = "AB";
    char* result4 = convert(case4, 1);
    printf("Result 4: '%s'\n", result4);
    free(result4);

    return 0;
}
