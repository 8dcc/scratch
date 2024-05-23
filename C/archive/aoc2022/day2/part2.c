
#include <stdio.h>

#define R1 'A'
#define P1 'B'
#define S1 'C'

#define R2 1
#define P2 2
#define S2 3

#define LOSS 'X'
#define TIE  'Y'
#define WIN  'Z'

// 'Z' (in part2 means win) to 6
#define CHAR2OUTCOME(c) ((c - 'X') * 3)

static int guess_char(char p1, char p2);

int main() {
    int total = 0;
    char c1 = 0, c2 = 0;

    while (scanf("%c %c\n", &c1, &c2) != EOF)
        total += CHAR2OUTCOME(c2) + guess_char(c1, c2);

    printf("Total: %d\n", total);

    return 0;
}

// Get p2 from p1 and outcome
int guess_char(char p1, char res) {
    switch (p1) {
        case R1:
            switch (res) {
                case WIN:
                    return P2;
                case TIE:
                    return R2;
                default:
                case LOSS:
                    return S2;
            }
        case P1:
            switch (res) {
                case WIN:
                    return S2;
                case TIE:
                    return P2;
                default:
                case LOSS:
                    return R2;
            }
        case S1:
            switch (res) {
                case WIN:
                    return R2;
                case TIE:
                    return S2;
                default:
                case LOSS:
                    return P2;
            }
        default:
            return 0;
    }
}
