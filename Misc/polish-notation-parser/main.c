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

#if 0
    /*--------------------------------------------------------------------*/
    /* FIXME: Delete */

    /* (+ 1 2 (* 3 4 5) 6 7) */
    Token* root        = token_new();
    root->type         = TOKEN_PARENT;
    root->val.children = malloc(7 * sizeof(Token));

    root->val.children[0].type    = TOKEN_OPERATOR;
    root->val.children[0].val.str = "+";
    root->val.children[1].type    = TOKEN_NUM;
    root->val.children[1].val.num = 1;
    root->val.children[2].type    = TOKEN_NUM;
    root->val.children[2].val.num = 2;

    root->val.children[3].type         = TOKEN_PARENT;
    root->val.children[3].val.children = malloc(5 * sizeof(Token));

    root->val.children[3].val.children[0].type    = TOKEN_OPERATOR;
    root->val.children[3].val.children[0].val.str = "*";
    root->val.children[3].val.children[1].type    = TOKEN_NUM;
    root->val.children[3].val.children[1].val.num = 3;
    root->val.children[3].val.children[2].type    = TOKEN_NUM;
    root->val.children[3].val.children[2].val.num = 4;
    root->val.children[3].val.children[3].type    = TOKEN_NUM;
    root->val.children[3].val.children[3].val.num = 5;
    root->val.children[3].val.children[4].type    = TOKEN_EOL;

    root->val.children[4].type    = TOKEN_NUM;
    root->val.children[4].val.num = 6;
    root->val.children[5].type    = TOKEN_NUM;
    root->val.children[5].val.num = 7;
    root->val.children[6].type    = TOKEN_EOL;

    puts("Print test:");
    tree_print(root, 0);
#endif

    /*--------------------------------------------------------------------*/

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
