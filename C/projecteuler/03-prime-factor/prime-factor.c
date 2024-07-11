#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAX_FACTORS 50

uint64_t largest_prime_factor(uint64_t target) {
    static uint64_t factors[MAX_FACTORS];
    int factors_pos = 0;

    for (uint64_t n = 2; n <= target; n++) {
        bool is_prime = true;
        for (int i = 0; i < factors_pos; i++) {
            if (n % factors[i] == 0) {
                is_prime = false;
                break;
            }
        }

        if (!is_prime)
            continue;

        if (target % n == 0) {
            factors[factors_pos++] = n;
            target /= n;
        }
    }

    return (factors_pos <= 0) ? target : factors[factors_pos - 1];
}

int main(void) {
    const uint64_t target = 600851475143;

    const uint64_t result = largest_prime_factor(target);
    printf("Result: %ld\n", result);

    return 0;
}
