
#include <stdio.h>

#define R1 'A'
#define P1 'B'
#define S1 'C'

#define R2 'X'
#define P2 'Y'
#define S2 'Z'

#define LOSS 0
#define TIE  3
#define WIN  6

#define CHAR2SCORE(c) (c - 'X' + 1)

static int outcome(char p1, char p2);

int main() {
    int total = 0;
    char c1 = 0, c2 = 0;

    while (scanf("%c %c\n", &c1, &c2) != EOF)
        total += CHAR2SCORE(c2) + outcome(c1, c2);

    printf("Total: %d\n", total);

    return 0;
}

int outcome(char p1, char p2) {
    switch (p1) {
        case R1:
            switch (p2) {
                case R2:
                    return TIE;
                case P2:
                    return WIN;
                default:
                case S2:
                    return LOSS;
            }
        case P1:
            switch (p2) {
                default:
                case R2:
                    return LOSS;
                case P2:
                    return TIE;
                case S2:
                    return WIN;
            }
        case S1:
            switch (p2) {
                case R2:
                    return WIN;
                default:
                case P2:
                    return LOSS;
                case S2:
                    return TIE;
            }
        default:
            return 0;
    }
}
