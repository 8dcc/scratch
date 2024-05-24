
#include <stdio.h>
#include <stdlib.h>

static void fizzbuzz(int iterations) {
    const char* p[2][2] = {
        /* !(%3)     (%3) */
        { "%d\n",   "Fizz\n" },       /* !(%5) */
        { "Buzz\n", "FizzBuzz\n" }, /* (%5) */
    };

    for (int i = 1; i <= iterations; i++)
        printf(p[i % 5 == 0][i % 3 == 0], i);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <iterations>\n", argv[0]);
        return 1;
    }

    const int iterations = atoi(argv[1]);
    fizzbuzz(iterations);

    return 0;
}
