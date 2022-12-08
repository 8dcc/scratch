
#include <stdio.h>
#include <string.h>

#define STACK_N  9
#define STACK_SZ 100

#define INPUT_STACK_LINES 8

static inline void move(int ammount, int from, int to);

// Create the stacks
char stack[STACK_N][STACK_SZ] = {
    { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 0 },
};

int main() {
    strcpy(stack[0], "STHFWR");
    strcpy(stack[1], "SGDQW");
    strcpy(stack[2], "BTW");
    strcpy(stack[3], "DRWTNQZJ");
    strcpy(stack[4], "FBHGLVTZ");
    strcpy(stack[5], "LPTCVBSG");
    strcpy(stack[6], "ZBRTWGP");
    strcpy(stack[7], "NGMTCJR");
    strcpy(stack[8], "LGBW");

    /*
    char buf_arr[255] = { 0 };
    char* buf         = buf_arr;

    for (int i = INPUT_STACK_LINES - 1; i >= 0; i--) {
        fgets(buf, 255, stdin);
        buf++;    // First '['

        for (int j = 0; j < STACK_N; j++) {
            stack[j][i] = (*buf == ' ') ? 0 : *buf;
            buf += 3;    // "] ["
        }
    }
    */

    int ammount = 0;
    int from    = 0;
    int to      = 0;
    while (scanf("move %d from %d to %d\n", &ammount, &from, &to) != EOF) {
        // -1 because stack 1 is idx 0
        move(ammount, from - 1, to - 1);
    }

    for (int i = 0; i < STACK_N; i++)
        for (int j = 0; j < STACK_SZ; j++)
            if (stack[i][j] != 0)
                printf("[%d][%d] = %c\n", i, j, stack[i][j]);

    printf("\nTop: ");
    for (int i = 0; i < STACK_N; i++) {
        for (int j = 0; j < STACK_SZ; j++) {
            if (stack[i][j] == 0) {
                if (j > 0)
                    printf("%c", stack[i][j - 1]);

                break;
            }
        }
    }
    putchar('\n');

    return 0;
}

void move(int ammount, int from, int to) {
    int from_p = 0, to_p = 0;

    // Get last positions of stack[to] and stack[from]
    while (stack[to][to_p] != 0)
        to_p++;
    if (to_p > 0)
        to_p--;
    while (stack[from][from_p] != 0)
        from_p++;
    if (from_p > 0)
        from_p--;

    // Make from_p point to the last (ammount) chars
    from_p -= ammount;

    // Move and set to 0
    for (int i = 0; i < ammount; i++) {
        stack[to][to_p]     = stack[from][from_p];
        stack[from][from_p] = 0;
    }
}
