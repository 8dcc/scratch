#include <stdio.h>

int main() {
    /* Assign pointer val to another */
    int num = 6969;
    int *p;     // Malloc
    *p = num;

    int **p2 = num;

    printf("SI: %d | SC: %d\n", sizeof(int), sizeof(char));
    printf("P:  %d -> %d\n", p, *p);
    printf("P2: %d -> %d\n", p2, *p2);

    return 0;
}
