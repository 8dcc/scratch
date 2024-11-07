#include <stdio.h>

void move_stack(int num, char from, char to, char aux) {
    if (num <= 0)
        return;

    move_stack(num - 1, from, aux, to);
    printf("Moving from %c to %c\n", from, to);
    move_stack(num - 1, aux, to, from);
}

int main(void) {
    move_stack(4, 'A', 'B', 'C');
    return 0;
}
