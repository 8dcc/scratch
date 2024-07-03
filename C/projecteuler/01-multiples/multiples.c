#include <stdbool.h>
#include <stdio.h>

bool valid_num(int n) {
    return n % 3 == 0 || n % 5 == 0;
}

int main(void) {
    int result = 0;

    for (int i = 0; i < 1000; i++)
        if (valid_num(i))
            result += i;

    printf("Result: %d\n", result);

    return 0;
}
