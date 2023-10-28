/*
 * For more information on parsers, see:
 *   https://supunsetunga.medium.com/writing-a-parser-getting-started-44ba70bb6cc9
 *   https://supunsetunga.medium.com/writing-a-parser-algorithms-and-implementation-a7c40f46493d
 *
 * Reads each line from stdin, and uses an in-memory reader.
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "parser.h"
#include "util.h"

int main(void) {
    puts("--- Polish notation ---");

    bool quit = false;
    while (!quit) {
        char* input = readline("Input> ");

        /* The readline() function detected EOF with an empty line */
        if (input == NULL)
            break;

        add_history(input);

        Token* tokens = parse(input);
        tree_print(tokens, 0);

        /* TODO: Free tokens using tree_free() */

        /* Free allocated memory from readline() */
        free(input);
    }

    return 0;
}
