/*
 * Copyright 2025 8dcc
 *
 * This file is part of scratch.
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

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h> /* log2() */

double entropy(void* data, size_t data_sz) {
    static int occurrences[256];

    /* Initialize the occurrences to zero */
    for (int i = 0; i < 256; i++)
        occurrences[i] = 0;

    /* Count the occurrences of each byte in the input */
    for (size_t i = 0; i < data_sz; i++) {
        const unsigned char byte = ((unsigned char*)data)[i];
        occurrences[byte]++;
    }

    double result = 0.0;

    /* Iterate each possible value (00..FF) */
    for (int byte = 0; byte < 256; byte++) {
        if (occurrences[byte] == 0)
            continue;

        /* Probablity of encountering this byte on the input */
        const double probability = (double)occurrences[byte] / data_sz;

        /*
         * The log2() multiplication is used to make sure that we get 0
         * "surprise" when the probability of the event is 1. For more
         * information, see my article about entropy:
         *   https://8dcc.github.io/programming/understanding-entropy.html
         *
         * Since the log2 of [0..1] is always negative, we subtract from the
         * total to increase its value.
         */
        result -= probability * log2(probability);
    }

    return result;
}

/*----------------------------------------------------------------------------*/

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        fprintf(stderr, "Can't open file: \"%s\"\n", argv[1]);
        return 1;
    }

    /* Get the file size */
    fseek(fp, 0L, SEEK_END);
    const size_t file_sz = ftell(fp);
    fseek(fp, 0L, SEEK_SET);

    char* file_content = malloc(file_sz);
    if (file_content == NULL) {
        fprintf(stderr, "Couldn't allocate %zu bytes.\n", file_sz);
        fclose(fp);
        return 1;
    }

    /* Store the file content in a buffer */
    for (size_t i = 0; i < file_sz; i++)
        file_content[i] = fgetc(fp);
    fclose(fp);

    /* Print the entropy */
    const double file_entropy = entropy(file_content, file_sz);
    printf("Entropy: %f\n", file_entropy);

    return 0;
}
