#include <stdio.h>

#define ADDRESS 6422036

int main() {
    int *p = (void *)ADDRESS;
    printf("Reading...\n");
    printf("Address: %d | Pointer: %d | Dereferenced: %d\n", ADDRESS, p, *p);
    printf("Press any key to end the program...\n");
    getchar();
    return 0;
}
