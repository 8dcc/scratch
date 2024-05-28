/* Kata: https://www.codewars.com/kata/557f6437bf8dcdd135000010 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Calculates factorials but the ulong is not big enough
char *factorial(int n) {
    if (n < 0) return NULL;

    unsigned long final_int = 1;
    for (long i = 1; i <= n; i++) {
        final_int *= i;
    }

    char* final = malloc(250);
    sprintf(final, "%lu", final_int);
    return final;
}

