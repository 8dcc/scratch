
#include <stdio.h>
#include <stdlib.h>

static void fizzbuzz_loop(int iterations) {
    for (int i = 1; i <= iterations; i++) {
        if (i % 15 == 0)
            puts("FizzBuzz");
        else if (i % 3 == 0)
            puts("Fizz");
        else if (i % 5 == 0)
            puts("Buzz");
        else
            printf("%d\n", i);
    }
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <iterations>\n", argv[0]);
        return 1;
    }

    const int iterations = atoi(argv[1]);
    fizzbuzz_loop(iterations);

    return 0;
}
