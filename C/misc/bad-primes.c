
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

int main() {
    uint32_t num = 1000000;

    uint32_t found = 0;
    for (uint32_t i = 1; i <= num; i++) {
        bool prime = true;

        /* Very basic method, used to test the performance of I/O functions */
        for (uint32_t j = 2; j < i; j++) {
            if (i % j == 0) {
                prime = false;
                break;
            }
        }

        if (prime) {
            printf("%d\n", i);
            found++;
        }
    }

    printf("Done. %d primes found from 1 to %d.\n", found, num);
}
