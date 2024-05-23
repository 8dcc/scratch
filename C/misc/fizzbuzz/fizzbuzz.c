#include <stdio.h>
#include <stdlib.h>

void fizzbuzz_loop(const int iterations);
const char* int2fizzbuzz(const int n);

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("I need the number of rounds!\n");
        return 1;
    }

    fizzbuzz_loop(atoi(argv[1]));       // Pass arg as int to func
    return 0;
}

void fizzbuzz_loop(const int iterations) {
    for (int n = 1; n < iterations; n++) {
        if (n % 15 == 0)        printf("FizzBuzz");
        else if (n % 3 == 0)    printf("Fizz");
        else if (n % 5 == 0)    printf("Buzz");
        else                    printf("%d", n);

        printf("\n");
    }
}
