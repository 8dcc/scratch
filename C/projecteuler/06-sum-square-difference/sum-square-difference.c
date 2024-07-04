#include <stdio.h>
#include <math.h>

int sum_squares(int limit) {
    int result = 0;
    for (int i = 0; i <= limit; i++)
        result += pow(i, 2);
    return result;
}

int square_sum(int limit) {
    int result = 0;
    for (int i = 0; i <= limit; i++)
        result += i;
    return pow(result, 2);
}

int main(void) {
    const int limit = 100;

    const int sum1 = sum_squares(limit);
    const int sum2 = square_sum(limit);
    printf("Sum1: %d\n", sum1);
    printf("Sum2: %d\n", sum2);

    const int result = sum2 - sum1;
    printf("Result: %d\n", result);

    return 0;
}
