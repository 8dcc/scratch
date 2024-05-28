/* Kata: https://www.codewars.com/kata/5672a98bdbdd995fad00000f */
/* Was nice to think of the best way of doing it */

enum tool {ROCK, PAPER, SCISSORS};
enum outcome {P1_WON, P2_WON, DRAW};

enum outcome rps (enum tool p1, enum tool p2)
{
    if (p1 == p2) return DRAW;
    switch (p1) {
        case ROCK:
            if (p2 == PAPER) return P2_WON;
            if (p2 == SCISSORS) return P1_WON;  // `else if` not needed
            break;
        case PAPER:
            if (p2 == ROCK) return P1_WON;
            if (p2 == SCISSORS) return P2_WON;
            break;
        case SCISSORS:
            if (p2 == ROCK) return P2_WON;
            if (p2 == PAPER) return P1_WON;
            break;
        default: break;     // Also not needed here but always better
    }
}
