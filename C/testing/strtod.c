
#include <stdio.h>
#include <stdlib.h> /* exit(), strtod() */

int main(void) {
    char input[100];
    printf("Input string: ");
    if (scanf("%100s", input) != 1) {
        fprintf(stderr, "Failed to read string.\n");
        exit(1);
    }

    char* endptr;
    double converted = strtod(input, &endptr);
    if (endptr == NULL || input == endptr) {
        fprintf(stderr, "Failed to convert string to double.\n");
        exit(1);
    }

    if (*endptr == '\0')
        printf("Fully converted: %f\n", converted);
    else
        printf("Partially converted: %f\n"
               "Rest of string: %s\n",
               converted, endptr);

    return 0;
}
