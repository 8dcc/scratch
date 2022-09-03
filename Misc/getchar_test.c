#include <stdio.h>
#include <stdlib.h>

void clear_input(const char current) {
    char c = current;

    // Clear newlines after input
    while (c != '\n' && c != EOF)
        c = getchar();
}

int main() {
    printf("Input 1: ");
    char a = getchar();
    clear_input(a);

    printf("Input 2: ");
    char b = getchar();
    clear_input(b);

    printf("\nInput 1: %c | Input 2: %c\n", a, b);

    return 0;
}
