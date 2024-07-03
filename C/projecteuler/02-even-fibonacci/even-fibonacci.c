#include <stdbool.h>
#include <stdio.h>

#define FIB_LIMIT 4000000

static inline bool is_even(unsigned int x) {
    /* Check if the lowest bit is set */
    return (x & 1) == 0;
}

int main(void) {
    int result = 0;

    unsigned int cur_fib = 1, last_fib = 1;
    while (cur_fib < FIB_LIMIT) {
        if (is_even(cur_fib))
            result += cur_fib;

        int aux = last_fib;
        last_fib = cur_fib;
        cur_fib += aux;
   }

    printf("Result: %d\n", result);

    return 0;
}
