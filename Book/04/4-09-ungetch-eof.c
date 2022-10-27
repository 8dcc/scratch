/* 4-09 */

#include <stdio.h>

int getch();
void ungetch(int c);

int main() {
    int c = 0;

    ungetch(EOF);

    // It will stop only when EOF is returned from getchar()
    while ((c = getch()) != EOF) putchar(c);

    return 0;
}

// We use EOF instead of 0 because we know its an invalid value
int getch_buf = EOF;

int getch() {
    const int old_buf = getch_buf;

    if (getch_buf != EOF) {
        getch_buf = EOF;
        return old_buf;
    } else {
        return getchar();
    }
}

void ungetch(int c) {
    getch_buf = c;
}

