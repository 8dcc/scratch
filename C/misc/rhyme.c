/*
 * Copyright 2025 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <errno.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ERR(...)                                                               \
    do {                                                                       \
        fprintf(stderr, __VA_ARGS__);                                          \
        fputc('\n', stderr);                                                   \
    } while (0)

/*----------------------------------------------------------------------------*/
/* Structures */

typedef struct Word {
    char* str;
    size_t len;

    /*
     * Ratio [0..1] between the number of matching characters with a target
     * word, and the length of the word.
     */
    float match;
} Word;

typedef struct Dict {
    Word* words;
    size_t size;
} Dict;

/*----------------------------------------------------------------------------*/
/* Misc helpers */

void* safe_malloc(size_t size) {
    void* result = malloc(size);
    if (result == NULL) {
        ERR("Could not allocate %zu bytes.", size);
        exit(1);
    }
    return result;
}

void safe_realloc(void* double_ptr, size_t size) {
    void** casted_double_ptr = (void**)double_ptr;
    void* result             = realloc(*casted_double_ptr, size);
    if (result == NULL) {
        ERR("Could not reallocate into %zu bytes.", size);
        exit(1);
    }
    *casted_double_ptr = result;
}

/*----------------------------------------------------------------------------*/
/* Dictionary initialization and deinitialization */

/*
 * Initialize and return a dictionary containing each non-empty line of the
 * specified file.
 */
static Dict dict_read(FILE* fp) {
    Dict result = {
        .words = NULL,
        .size  = 0,
    };

    size_t dict_pos     = 0;
    size_t dict_sz      = 0;
    size_t cur_word_pos = 0;
    size_t cur_word_sz  = 0;

    int c;
    while ((c = fgetc(fp)) != EOF) {
        if (c == '\n') {
            /*
             * We encountered a newline, but our current word buffer is empty.
             * Just ignore the newline.
             */
            if (cur_word_pos == 0) {
                assert(cur_word_sz == 0);
                continue;
            }

            /*
             * We encountered a newline, and our current word buffer is not
             * empty. Store the real word length, reset the word position, and
             * move to the next dictionary position (which will potentially hold
             * a new word buffer).
             */
            assert(cur_word_pos <= cur_word_sz);
            result.words[dict_pos].len   = cur_word_pos;
            result.words[dict_pos].match = 0.f;
            cur_word_pos = cur_word_sz = 0;
            dict_pos++;
            continue;
        }

        /*
         * If we reached this point, we want to write the contents of a word.
         *
         * If the current word is empty, first check if there is space for
         * the buffer pointer itself in the 'words' array. If not, expand
         * it. Then, set the initial word size (an arbitrary guess) and allocate
         * the buffer.
         *
         * Otherwise, if the current word is full, duplicate the current word
         * size and reallocate the buffer.
         */
        if (cur_word_pos == 0) {
            assert(cur_word_sz == 0);
            if (dict_pos >= dict_sz) {
                dict_sz = (dict_sz == 0) ? 100 : dict_sz * 2;
                safe_realloc(&result.words, dict_sz * sizeof(Word));
            }
            cur_word_sz = 10;
            result.words[dict_pos].str =
              safe_malloc(cur_word_sz * sizeof(char));
        } else if (cur_word_pos >= cur_word_sz) {
            assert(cur_word_sz > 0);
            cur_word_sz *= 2;
            safe_realloc(&result.words[dict_pos], cur_word_sz * sizeof(char));
        }

        /*
         * If we reached this point, we know we have a valid buffer for writing
         * our character.
         */
        result.words[dict_pos].str[cur_word_pos++] = c;
    }

    /*
     * In case the file didn't end with a newline, terminate the last word.
     */
    if (cur_word_pos != 0) {
        assert(cur_word_pos <= cur_word_sz);
        result.words[dict_pos].len = cur_word_pos;
    }

    result.size = dict_pos;
    return result;
}

/*
 * Free all elements of a dictionary.
 */
static void dict_free(Dict dict) {
    for (size_t i = 0; i < dict.size; i++)
        free(dict.words[i].str);
    free(dict.words);
}

/*----------------------------------------------------------------------------*/
/* Dictionary matching */

/*
 * Return the number of rhyme matches between A and B, starting from the end.
 */
static inline int rightmost_matches(Word a, Word b) {
    const char* a_left  = a.str;
    const char* a_right = &a.str[a.len - 1];
    const char* b_left  = b.str;
    const char* b_right = &b.str[b.len - 1];

    int result = 0;
    while (a_left <= a_right && b_left <= b_right && *a_right == *b_right) {
        result++;
        a_right--;
        b_right--;
    }

    return result;
}

/*
 * Return the number of potential rhyme matches in a word.
 */
static inline int potential_matches(Word word) {
    return word.len;
}

/*
 * Store the "match ratio" of each word in the dictionary when compared to a
 * target.
 *
 * The "match ratio" of a word and a target is calculated by dividing the number
 * of right-most matching characters between the two strings (according to the
 * 'rightmost_matches' function) by the length of the target.
 *
 * For example, a word "aaabbb" and a target "bb" would have a 1.0 ratio, since
 * all characters of the target match the right-most characters of the word.
 */
static void dict_match(Dict dict, const char* target) {
    const Word target_word = {
        .str = (char*)target,
        .len = strlen(target),
    };

    for (size_t i = 0; i < dict.size; i++) {
        const Word cur_word = dict.words[i];
        const int match_num = rightmost_matches(target_word, cur_word);
        const int potential = potential_matches(target_word);
        dict.words[i].match = (float)match_num / potential;
    }
}

/*----------------------------------------------------------------------------*/
/* Dictionary sorting */

/*
 * Swap the specified words.
 */
static inline void word_swap(Word* a, Word* b) {
    static Word tmp;
    tmp = *a;
    *a  = *b;
    *b  = tmp;
}

/*
 * Check if the word A should appear before the word B or not.
 */
static inline bool word_is_sorted(const Word* a, const Word* b) {
    return (a->match >= b->match);
}

/*
 * Sort the specified dictionary using the Insertion Sort algorithm.
 */
static void dict_sort(Dict dict) {
    for (size_t i = 0; i < dict.size; i++) {
        /*
         * Check how the current element compares to the previous one. If it
         * needs to be sorted, keep moving to the front.
         */
        for (size_t j = i; j > 0; j--) {
            Word* prev = &dict.words[j - 1];
            Word* cur  = &dict.words[j];
            if (word_is_sorted(prev, cur))
                break;
            word_swap(prev, cur);
        }
    }
}

/*----------------------------------------------------------------------------*/
/* Dictionary printing */

static inline void word_print(Word word) {
    for (size_t i = 0; i < word.len; i++)
        putchar(word.str[i]);
}

/*
 * Print all possible information about a dictionary.
 */
static inline void dict_print(Dict dict) {
    for (size_t i = 0; i < dict.size; i++) {
        printf("[%zu] ", i);
        word_print(dict.words[i]);
        printf(" (%.2f)\n", dict.words[i].match);
    }
}

static inline void dict_print_matches(Dict dict, float match_threshold) {
    for (size_t i = 0; i < dict.size; i++) {
        if (dict.words[i].match < match_threshold)
            continue;

        printf("(%.2f) ", dict.words[i].match);
        word_print(dict.words[i]);
        putchar('\n');
    }
}

/*----------------------------------------------------------------------------*/
/* Entry point */

int main(int argc, char** argv) {
    if (argc != 3) {
        ERR("Usage: %s DICT_FILE TARGET_WORD", argv[0]);
        return 1;
    }

    const char* dict_pathname = argv[1];
    const char* target_word   = argv[2];

    FILE* dict_fp = fopen(dict_pathname, "r");
    if (dict_fp == NULL) {
        ERR("Could not open '%s': %s", dict_pathname, strerror(errno));
        return 1;
    }

    Dict dict = dict_read(dict_fp);
    dict_match(dict, target_word);
    dict_sort(dict);
    dict_print_matches(dict, 0.1);

    dict_free(dict);
    fclose(dict_fp);
    return 0;
}
