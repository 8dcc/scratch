
#include <stdio.h>

char* itoa(char* s, int n);

int main() {
    int i = 4321;
    char s[10] = {0};

    itoa(s, i);
    printf("i: %d | itoa: %s\n", i, s);

    return 0;
}

