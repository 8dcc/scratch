#include <stdio.h>

int sum_digits(const int n);

int main() {
    // Tests
    int num1 = 123;     // 6
    printf("%5d | %d\n", num1, sum_digits(num1));
    int num2 = 76671;   // 27 -> 9
    printf("%5d | %d\n", num2, sum_digits(num2));

    return 0;
}

/* sum_digits: Keep adding the digits of an int until there is only one left */
int sum_digits(const int n) {
    int buf = n;
    int final = 0;
    while (buf > 9) {
        final = 0;
        while (buf >= 1) {
            final += buf % 10;
            buf = buf / 10;
        }
        buf = final;
    }
    return final;
}
