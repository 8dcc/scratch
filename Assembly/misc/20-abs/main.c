
#include <stdio.h>

/* From abs.asm */
extern int my_abs(int n);

int main(void) {
    const int n1 = -123;
    printf("my_abs(%d) -> %d\n", n1, my_abs(n1));

    const int n2 = 456;
    printf("my_abs(%d) -> %d\n", n2, my_abs(n2));

    return 0;
}
