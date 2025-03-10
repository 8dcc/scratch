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

typedef struct Word {
    char* str;
    size_t len;
} Word;

typedef struct Dict {
    Word* words;
    size_t size;
} Dict;

/*----------------------------------------------------------------------------*/

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

/*
 * Initialize and return a dictionary containing each non-empty line of thje
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
            result.words[dict_pos].len = cur_word_pos;
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

/*
 * Print all possible information about a dictionary.
 */
static inline void dict_print(Dict dict) {
    for (size_t i = 0; i < dict.size; i++) {
        printf("[%zu] ", i);
        for (size_t j = 0; j < dict.words[i].len; j++)
            putchar(dict.words[i].str[j]);
        putchar('\n');
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        ERR("Usage: %s DICT-FILE", argv[0]);
        return 1;
    }

    const char* dict_pathname = argv[1];
    FILE* dict_fp             = fopen(dict_pathname, "r");
    if (dict_fp == NULL) {
        ERR("Could not open '%s': %s", dict_pathname, strerror(errno));
        return 1;
    }

    /*
     * NOTE: The current program doesn't operate on the dictionary, it just
     * prints it.
     */
    Dict dict = dict_read(dict_fp);
    dict_print(dict);

    dict_free(dict);
    fclose(dict_fp);
    return 0;
}
