
#include <stdio.h>
#include <stdlib.h> /* atoi() */

/* From sieve-of-eratosthenes.asm */
extern void sieve(int iterations);

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <iterations>\n", argv[0]);
        return 1;
    }

    const int iterations = atoi(argv[1]);
    if (iterations <= 0) {
        fprintf(stderr, "Invalid number of iterations\n");
        return 1;
    }

    /* Sieve will generate and print the primes */
    sieve(iterations);

    return 0;
}
