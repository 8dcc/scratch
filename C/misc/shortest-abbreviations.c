
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

static size_t read_words(FILE* fp, char*** words_ptr) {
    size_t words_i  = 0;
    size_t words_sz = 100;
    char** words    = malloc(words_sz * sizeof(char*));

    int c = fgetc(fp);
    for (;;) {
        if (words_i >= words_sz) {
            words_sz += 100;
            words = realloc(words, words_sz);
        }

        /* Skip leading spaces */
        while (isspace(c))
            c = fgetc(fp);
        if (c == EOF)
            break;

        /* Store characters of word itself, until space */
        size_t word_i  = 0;
        size_t word_sz = 100;
        char* word     = malloc(word_sz);
        for (; !isspace(c); c = fgetc(fp)) {
            if (word_i + 1 >= word_sz) {
                word_sz += 100;
                word = realloc(word, word_sz);
            }

            word[word_i++] = c;
        }
        word[word_i] = '\0';

        /* Store the pointer to the word we just filled */
        words[words_i++] = word;
    }

    /*
     * Write the address of the array of words, and return the total number of
     * words read.
     */
    *words_ptr = words;
    return words_i;
}

static void free_words(char** words, size_t words_sz) {
    for (size_t i = 0; i < words_sz; i++)
        free(words[i]);
    free(words);
}

/*----------------------------------------------------------------------------*/

static size_t get_abbrev_sz(char** words, size_t words_sz, size_t target_idx) {
    const char* target = words[target_idx];
    size_t target_len  = strlen(target);

    /*
     * The `abbrev_sz' index is valid for abbreviations if there are no other
     * words that match the `target' up to that `abbrev_sz' index.
     *
     * We check each word, and look for matches. If there are none, we found the
     * string.
     *
     * If the whole `target' matches another string (i.e. it's repeated in
     * `words'), the index of the last character is returned.
     */
    size_t abbrev_sz;
    bool abbrev_has_match = true;
    for (abbrev_sz = 1; abbrev_sz <= target_len && abbrev_has_match;
         abbrev_sz++) {
        abbrev_has_match = false;
        for (size_t i = target_idx + 1; i < words_sz; i++) {
            if (!strncmp(target, words[i], abbrev_sz)) {
                abbrev_has_match = true;
                break;
            }
        }
    }

    return abbrev_sz - 1;
}

static void print_abbreviations(FILE* fp, char** words, size_t words_sz) {
    for (size_t i = 0; i < words_sz; i++) {
        const size_t abbrev_sz = get_abbrev_sz(words, words_sz, i);
        for (size_t j = 0; j < abbrev_sz; j++)
            fputc(words[i][j], fp);
        fputc('\n', fp);
    }
}

/*----------------------------------------------------------------------------*/

int main(void) {
    char** words;
    size_t words_sz = read_words(stdin, &words);

    print_abbreviations(stdout, words, words_sz);

    free_words(words, words_sz);
    return 0;
}
