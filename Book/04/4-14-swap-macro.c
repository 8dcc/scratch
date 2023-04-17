/* 4 - 14 */

#include <stdio.h>

#define swap(t, x, y) \
    {                 \
        t tmp = x;    \
        x     = y;    \
        y     = tmp;  \
    }

int main() {
    int n1 = 123;
    int n2 = 789;

    printf("%d, %d -> ", n1, n2);
    swap(int, n1, n2);
    printf("%d, %d\n", n1, n2);
}
