#include <stdio.h>

typedef int (*FuncPtr)(int x);

int summation(int a, int b, FuncPtr f) {
    int ret = 0;

    for (; a <= b; a++)
        ret += f(a);

    return ret;
}

int echo(int x) {
    return x;
}

int pow2(int x) {
    return (1 << x); /* (2 ^ x) */
}

int main(void) {
    printf("%d, ", summation(1, 5, echo));
    printf("%d\n", summation(1, 5, pow2));
}
