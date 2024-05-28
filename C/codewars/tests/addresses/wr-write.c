#include <stdio.h>

int main() {
    int num = 223;
    int *p = &num;
    printf("Address: %d | Pointer: %d | Dereferenced: %d\n", &num, p, *p);
    printf("Press any key to end the program...\n");
    getchar();
    return 0;
}
