/* 3-02 */

#include <stdio.h>

char* escape(char* dest, char* src);

int main() {
    char s[] = "Balls\tBalls\tBalls...\n"
               "Balls?    \tBalls.\n";
    char e[255] = { 0 };

    escape(e, s);

    printf("Original:\n"
           "%s\n"
           "-----------------------\n"
           "Escape:\n"
           "%s\n", s, e);

    return 0;
}

/*
 * Replaces escaped secuences like '\n' from src with literally "\n".
 * Returns a pointer to dest.
 */
char* escape(char* dest, char* src) {
    int dp = 0;     /* Current character pos in dest str */

    for (int n = 0; src[n] != '\0'; n++) {
        switch (src[n]) {
            case '\n':
                dest[dp++] = '\\';
                dest[dp++] = 'n';
                break;
            case '\t':
                dest[dp++] = '\\';
                dest[dp++] = 't';
                break;
            case ' ':
                dest[dp++] = 250;   /* · */
                break;
            default:
                dest[dp++] = src[n];
                break;
        }
    }
    
    return dest;
}
