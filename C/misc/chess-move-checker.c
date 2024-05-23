#include <stdbool.h>
#include <stdio.h>

#define X      1
#define ABS(n) ((n) < 0 ? (-(n)) : (n))

enum dirs {
    ROOK   = 0,
    BISHOP = 1,
    QUEEN  = 2,
};

enum errors {
    INVALID = 0,
    VALID   = 1,
    PIECE   = 2,
    UNKNOWN = 3,
};

bool has_piece(int x, int y) {
    /* TODO: This is just an example */
    static const bool test_arr[8][8] = {
        { 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0, 1, 0, 0 }, /* at x5, y1 */
        { 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0, X, 0, 0 }, /* we are x5, y5 */
        { 0, 0, 0, 0, 1, 0, 0, 0 }, /* at x4, y6 */
        { 0, 0, 0, 0, 0, 0, 0, 0 },
    };

    return test_arr[y][x];
}

/* NOTE: Doesn't check for pieces at the last position (new_x, new_y) */
int valid_move(int movetype, int piece_x, int piece_y, int new_x, int new_y) {
    switch (movetype) {
        case ROOK:
            if (new_x == piece_x) {
                const int y_step = new_y > piece_y ? 1 : -1;

                for (int y = piece_y + y_step; y != new_y; y += y_step)
                    if (has_piece(piece_x, y))
                        return PIECE;
                return VALID;
            } else if (new_y == piece_y) {
                const int x_step = new_x > piece_x ? 1 : -1;

                for (int x = piece_x + x_step; x != new_x; x += x_step)
                    if (has_piece(x, piece_y))
                        return PIECE;
                return VALID;
            } else {
                return INVALID;
            }
        case BISHOP:
            if (ABS(new_x - piece_x) != ABS(new_y - piece_y))
                return INVALID;

            const int x_step = new_x > piece_x ? 1 : -1;
            const int y_step = new_y > piece_y ? 1 : -1;

            for (int x = piece_x + x_step; x != new_x; x += x_step)
                for (int y = piece_y + y_step; y != new_y; y += y_step)
                    if (has_piece(x, y))
                        return PIECE;
            return VALID;
        case QUEEN:
            const int rook_move =
              valid_move(ROOK, piece_x, piece_y, new_x, new_y);
            if (rook_move == INVALID)
                return valid_move(BISHOP, piece_x, piece_y, new_x, new_y);
            else
                return rook_move;
        default:
            return UNKNOWN;
    }
}

#define PRINT_EXPR(c) printf("%30s: %d\n", #c, c);

int main() {
    printf("{ 0,0,0,0,0,0,0,0 },\n"
           "{ 0,0,0,0,0,1,0,0 }, (at x5, y1)\n"
           "{ 0,0,0,0,0,0,0,0 },\n"
           "{ 0,0,0,0,0,0,0,0 },\n"
           "{ 0,0,0,0,0,0,0,0 },\n"
           "{ 0,0,0,0,0,X,0,0 }, (we are x5, y5)\n"
           "{ 0,0,0,0,1,0,0,0 }, (at x4, y6)\n"
           "{ 0,0,0,0,0,0,0,0 }\n\n");

    /* Valid moves */
    PRINT_EXPR(valid_move(ROOK, 5, 5, 3, 5));
    PRINT_EXPR(valid_move(BISHOP, 5, 5, 7, 7));
    PRINT_EXPR(valid_move(QUEEN, 5, 5, 6, 5));

    /* Invalid moves */
    PRINT_EXPR(valid_move(ROOK, 5, 5, 6, 6));
    PRINT_EXPR(valid_move(BISHOP, 5, 5, 6, 7));
    PRINT_EXPR(valid_move(QUEEN, 5, 5, 7, 2));

    /* Pieces */
    PRINT_EXPR(valid_move(ROOK, 5, 5, 5, 0));
    PRINT_EXPR(valid_move(BISHOP, 5, 5, 3, 7));
    PRINT_EXPR(valid_move(QUEEN, 5, 5, 3, 7));

    return 0;
}
