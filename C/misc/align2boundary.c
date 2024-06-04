
#include <stdio.h>

/* Align a number `n' to a `boundary', as long as the boundary is a power of
 * two. See: https://8dcc.github.io/reversing/challenge10.html#c-translation */
int align2boundary(int n, int boundary) {
    return (n + boundary - 1) & ~(boundary - 1);
}

int main(void) {
    printf("%#x\n", align2boundary(0x0123, 0x1000));
    printf("%#x\n", align2boundary(0x1234, 0x1000));
    printf("%#x\n", align2boundary(0x4567, 0x1000));

    return 0;
}
