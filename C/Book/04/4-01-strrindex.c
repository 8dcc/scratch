/* 4-01 */

#include <stdio.h>
#include <string.h>

#define CTX_MARGIN 30    // Used when printing the line and the '^'

int strrindex(char* str, char* pat);

int main() {
    /* Jay-Z - It's alright */
    char* str = "Bounce if you want to bounce, ball if you want to ball\n"
                "Play if you want to play, floss if you want to floss\n"
                "It's alright, you heard? It's alright, holla back\n"
                "We get ill if you want to ill, smoke if you want to smoke\n";
    char* pat = "smoke";

    int idx = strrindex(str, pat);
    if (idx == -1) {
        printf("Pattern '%s' not found.\n", pat);
        return 1;
    }

    printf("Last ocurrence of '%s' found at index: %d\n", pat, idx);

    printf("...");
    for (int i = idx - CTX_MARGIN; i < idx + CTX_MARGIN && str[i] != '\0'; i++)
        putchar(str[i]);
    /* putchar('\n'); */
    for (int i = idx - CTX_MARGIN - 3; i < idx; i++) putchar(' ');
    putchar('^');
    putchar('\n');

    return 0;
}

/* Searches pat in str, and returns the rightmost ocurrence. -1 if none */
int strrindex(char* str, char* pat) {
    const int pl = strlen(pat);

    int mp = 0;     // Matched pos. Will keep track of how many chars we have matched
                    // in each match.
    int fmp = 0;    // First matched pos. Will keep track of the first character in
                    // the current matched pattern.
    int lm = -1;    // Last match. Index of the first character of the last matched
                    // pattern. Starts at -1 because it will be returned if none is
                    // found.

    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] == pat[mp]) {
            mp++;

            // If this is the first matched char of the pattern, save it
            if (mp == 1) fmp = i;

            // If we matched the whole pattern
            if (mp == pl) lm = fmp;
        } else {
            mp  = 0;
            fmp = 0;
        }
    }

    return lm;
}
