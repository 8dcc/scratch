
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

        /* TODO: Improve explanation of what the log2() multiplication
         * accomplishes. */
        /* Since the log2 of [0..1] is always negative, we subtract from the
         * total to increase its value. */
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
    if (!fp) {
        fprintf(stderr, "Can't open file: \"%s\"\n", argv[1]);
        return 1;
    }

    /* Get the file size */
    fseek(fp, 0L, SEEK_END);
    const size_t file_sz = ftell(fp);
    fseek(fp, 0L, SEEK_SET);

    char* file_content = malloc(file_sz);
    if (!file_content) {
        fprintf(stderr, "Couldn't allocate %zu bytes.\n", file_sz);
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
