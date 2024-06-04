
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

int main(void) {
    int num = 1000000;

    int found = 0;
    for (int i = 1; i <= num; i++) {
        bool prime = true;

        /* Very basic method, used to test the performance of I/O functions */
        for (int j = 2; j < i; j++) {
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
    return 0;
}
