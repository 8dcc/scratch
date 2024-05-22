
#include <stdio.h>

extern int my_switch_test(int x);

int main() {
    /*
     *  1 -> 0x100
     *  2 -> 0x100
     *  3 -> 0x30
     *  4 -> 0x40
     *  5 -> 0x50
     *  6 -> 0x100
     *  7 -> 0x100
     *  8 -> 0x100
     *  9 -> 0x90
     * 10 -> 0x100
     */
    for (int i = 1; i <= 10; i++) {
        int result = my_switch_test(i);
        printf("%2d -> 0x%X\n", i, result);
    }

    return 0;
}
