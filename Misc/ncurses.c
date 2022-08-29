/*
 * Porgram to mess with ncurses. Quit with 'q'.
 * Compile using:
 *     gcc -o ncurses.out -Wall ncurses.c -lncurses
 */

#include <stdio.h>
#include <ctype.h>      // For tolower()
#include <ncurses.h>
#include <stdlib.h>     // For rand()
#include <time.h>       // For the seed

#define GRID_X 5
#define GRID_Y 2
#define GRID_C 10   // Number of items

#define REFRESH_0() move(0,0); refresh();

void draw_grid();                           // Will use defines instead of parameters
void fill_cell(int idx, const char c);      // Fills the idx with the specified char

int main() {
    initscr();      // Init ncurses

    draw_grid();

    do {
        // Fill grid with random chars
        srand(time(NULL));
        for (int n = 0; n < GRID_C; n++) {
            fill_cell(n, rand() % 26 + 'a');
        }
    } while (tolower(getchar()) != 'q');        // Repeat until we press 'q'
    
    endwin();       // End ncurses window
    return 0;
}

void draw_grid() {
    const int x = GRID_X;       // Define in case we want to pass this as args in the future
    const int y = GRID_Y;
    const int cells = GRID_C;

    if (cells < 1) return;

    // Initial left border
    mvprintw(y,   x, "+");
    mvprintw(y+1, x, "|");
    mvprintw(y+2, x, "+");

    for (int n = 0; n < cells; n++) {
        /* Center of each cell. We use +1 because of the initial left border and 2*n because
         * its the space of a cell times the current cell number:
         *   -+-+-+
         *    | | |
         *   -+-+-+
         *   ^^
         */
        mvprintw(y,   x+1+2*n, "-");
        mvprintw(y+1, x+1+2*n, " ");
        mvprintw(y+2, x+1+2*n, "-");

        mvprintw(y,   x+2+2*n, "+");
        mvprintw(y+1, x+2+2*n, "|");
        mvprintw(y+2, x+2+2*n, "+");
    }

    REFRESH_0();
}

void fill_cell(int idx, const char ch) {
    const int gx = GRID_X;
    const int gy = GRID_Y;
    const int gc = GRID_C;

    if (idx >= gc) return;

    mvprintw(gy+1, gx+1+2*idx, "%c", ch);

    REFRESH_0();
}
