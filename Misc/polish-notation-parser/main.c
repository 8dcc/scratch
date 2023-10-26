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

/*----------------------------------------------------------------------------*/

/* Returns true if string `str` mathes regex pattern `pat`. Pattern uses BRE
 * syntax: https://www.gnu.org/software/sed/manual/html_node/BRE-syntax.html */
static bool regex(const char* str, const char* pat) {
    static regex_t r;

    /* Compile regex pattern ignoring case */
    if (regcomp(&r, pat, REG_EXTENDED | REG_ICASE)) {
        fprintf(stderr,
                "plumber: regex: regcomp returned an error code for pattern "
                "\"%s\"\n",
                pat);
        return false;
    }

    int code = regexec(&r, str, 0, NULL, 0);
    if (code > REG_NOMATCH) {
        char err[100];
        regerror(code, &r, err, sizeof(err));
        fprintf(stderr, "plumber: regex: regexec returned an error: %s\n", err);
        return false;
    }

    /* REG_NOERROR: Success
     * REG_NOMATCH: Pattern did not match */
    return code == REG_NOERROR;
}

/*----------------------------------------------------------------------------*/

int main(void) {
    puts("--- Polish notation ---");

    bool quit = false;
    while (!quit) {
        char* input = readline("Input> ");

        /* The readline() function detected EOF with an empty line */
        if (input == NULL)
            break;

        add_history(input);

        if (!strcmp(input, "quit")) {
            quit = true;
            goto exit_loop;
        }

        /* FIXME */
        input_init(input);

        int tokens = token_count();
        printf("Count: %d\n", tokens);

    exit_loop:
        /* Free allocated memory from readline() */
        free(input);
    }

    return 0;
}
