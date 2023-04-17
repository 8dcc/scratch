/* 4-07 */

#include <stdio.h>

int getch();
void ungetch(int c);
void ungets(const char* s);

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

#define BUFSIZE 255
char ch_buf[BUFSIZE];
int ch_buf_p = 0;    // Next free pos on ch_buf

int getch() {
    return (ch_buf_p > 0) ? ch_buf[--ch_buf_p] : getchar();
}

void ungetch(int c) {
    ch_buf[ch_buf_p++] = c;
}

void ungets(const char* s) {
    for (int i = 0; s[i] != '\0' && i < BUFSIZE; i++) ungetch(s[i]);
}
