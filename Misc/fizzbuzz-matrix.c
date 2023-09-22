#include <stdio.h>

int main() {
    const char* p[2][2] = {
        /* !(%3)     (%3) */
        { "%d\n",   "Fizz\n" },     /* !(%5) */
        { "Buzz\n", "FizzBuzz\n" }, /* (%5) */
    };

    for (int i = 0; i < 100; i++)
        printf(p[i % 5 == 0][i % 3 == 0], i);

    return 0;
}
