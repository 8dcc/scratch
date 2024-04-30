
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

volatile int test_var = 5;

int main(void) {
    for (int i = 0; i < 1000; i++) {
        printf("test_var: %d\n", test_var);
        sleep(1);
    }

    return 0;
}
