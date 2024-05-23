
#include <stdio.h>
#include <stdlib.h>

int main() {
    int total = 0;    // Total calories of the current elf
    int max   = 0;    // Total calories of the max elf

    char buf[255] = { 0 };
    while (fgets(buf, 255, stdin)) {
        // Empty line
        if (buf[0] == '\n') {
            if (total > max)
                max = total;
            total = 0;
        } else {
            total += atoi(buf);
        }
    }

    printf("Max: %d\n", max);

    return 0;
}
