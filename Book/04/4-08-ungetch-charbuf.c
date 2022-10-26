/* 4-08 */

#include <stdio.h>

int getch();
void ungetch(int c);

int main() {
    int c = 0;

    printf("Getting 5 chars: ");
    for (int i = 0; i < 5; i++) {
        c = getch();
        putchar(c);
    }

    printf("\nUngetting '%c' 2 times...\n", c);
    for (int i = 0; i < 2; i++) ungetch(c);

    printf("Getting 8 chars: ");
    for (int i = 0; i < 8; i++) {
        c = getch();
        putchar(c);
    }

    putchar('\n');

    return 0;
}

// Could probably use another value to know when to actually do getch from buffer
// instead of checking if its 0, but whatever.
int getch_buf = 0;

int getch() {
    const int old_buf = getch_buf;

    if (getch_buf != 0) {
        getch_buf = 0;
        return old_buf;
    } else {
        return getchar();
    }
}

void ungetch(int c) {
    // Add here possible 2nd variable to indicate that there is a valid char there
    // (including 0 as valid if we wanted to)
    getch_buf = c;
}

