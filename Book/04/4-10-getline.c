/* 4-10 */

#include <stdio.h>

#define WORD_SZ 100

int get_line();    // Underscore because of stdio
int get_word(char* dest);

int main() {
    char word[WORD_SZ] = { 0 };

    printf("Getting line...\n");
    get_line();
    printf("Printing words:\n"
           "[ ");
    // While we get a word
    while (get_word(word) != 0) printf("\"%s\", ", word);
    printf(" ]\n");

    printf("Getting 2nd line...\n");
    get_line();
    printf("Printing words:\n"
           "[ ");
    // While we get a word
    while (get_word(word) != 0) printf("\"%s\", ", word);
    printf(" ]\n");

    return 0;
}

// Main won't need this as it will use get_word(s)
#define L_SZ 100
char l[L_SZ] = { 0 };
int lp       = 0;

// Used for indicating the last word pos used by get_word(). I could use something
// like a static, but I am using globals.
int word_p = 0;

/* get_line: saves chars from stdin to l[]. Returns length of line */
int get_line() {
    int c;
    lp     = 0;
    word_p = 0;

    // L_SZ - 1 to reserve space for the null terminator
    while ((c = getchar()) != '\n' && c != EOF && lp < L_SZ - 1) l[lp++] = c;

    l[lp] = '\0';

    return lp;
}

/*
 * get_word: saves the next word from l starting from word_p.
 * Returns length of word.
 */
int get_word(char* dest) {
    int i;

    for (i = 0; l[word_p] != ' ' && l[word_p] != '\0' && i < WORD_SZ; i++)
        dest[i] = l[word_p++];

    if (l[word_p] == ' ')
        word_p++;

    dest[i] = '\0';

    return i;
}

// We don't need ungetch anymore
