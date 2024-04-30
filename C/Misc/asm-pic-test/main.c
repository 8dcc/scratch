
#include <stdio.h>

/* Assembly function, calls foo() */
extern int foo_proxy(int a, int b);

int myGlobalInteger = 0;

int foo(int a, int b) {
    return a + b;
}

int main(void) {
    int result = foo_proxy(5, 6);
    printf("result: %d\n", result);
    printf("global: %d\n", myGlobalInteger);

    return 0;
}
