/* 5-07 */

#include <stdio.h>

#define MAXLINE  200
#define MAXLINES 15
#define ALLOCSZ  MAXLINE* MAXLINES

int readlines(char** arr, char* abuf, int maxlines);
int getline_c(char* s);

int main() {
    char allocarr[ALLOCSZ]  = { 0 };
    char* linearr[MAXLINES] = { 0 };

    int lines = readlines(linearr, allocarr, MAXLINES);

    for (int i = 0; i < lines; i++)
        printf("%2d: %s\n", i, linearr[i]);

    return 0;
}

/*
 * readlines: reads lines from stdin, stores them in abuf, and stores the line
 * pointers in arr. Returns the lines it got.
 */
int readlines(char** arr, char* abuf, int maxlines) {
    int line_c       = 0;
    int cur_line_len = 0;

    // Get the current line from stdin and save in on abuf
    while ((cur_line_len = getline_c(abuf)) != EOF && line_c <= maxlines) {
        // Next item of the line arr is the pointer to the current line we just got
        // from getline_c.
        *arr++ = abuf;

        // Add the last line length to abuf so the next time getline_c is called it
        // points to the next free space in abuf. We need +1 for the null terminator.
        abuf += cur_line_len + 1;

        line_c++;
    }

    return line_c;
}

/*
 * getline_c: read line from stdin and saves it in s. Returns the length of the
 * string it got (idx of null char).
 */
int getline_c(char* s) {
    int c  = 0;
    int sl = 0;

    while ((c = s[sl] = getchar()) != '\n' && c != EOF && sl < MAXLINE)
        sl++;

    s[sl] = '\0';

    // If we got eof, return that, if not return the len
    return (c == EOF) ? EOF : sl;
}
