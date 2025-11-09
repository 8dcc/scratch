
#include <stdio.h>

#define CLAMP(N, MIN, MAX)                                                     \
    (((MIN) >= (N)) ? (MIN) : ((N) >= (MAX)) ? (MAX) : (N))

/*----------------------------------------------------------------------------*/

#define PRINT_EXPR(EXPR) printf("%-16s -> %d\n", #EXPR, EXPR);

int main(void) {
    PRINT_EXPR(CLAMP(5, 1, 10));
    PRINT_EXPR(CLAMP(1, 1, 10));
    PRINT_EXPR(CLAMP(10, 1, 10));
    PRINT_EXPR(CLAMP(0, 1, 10));
    PRINT_EXPR(CLAMP(11, 1, 10));
    putchar('\n');
    PRINT_EXPR(CLAMP(5, 10, 1));
    PRINT_EXPR(CLAMP(1, 10, 1));
    PRINT_EXPR(CLAMP(10, 10, 1));
    PRINT_EXPR(CLAMP(0, 10, 1));
    PRINT_EXPR(CLAMP(11, 10, 1));

    return 0;
}
