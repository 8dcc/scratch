/*
 * For more information on parsers, see:
 *   https://supunsetunga.medium.com/writing-a-parser-getting-started-44ba70bb6cc9
 *   https://supunsetunga.medium.com/writing-a-parser-algorithms-and-implementation-a7c40f46493d
 *
 * Reads each line from stdin, and uses an in-memory reader.
 */

#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <readline/readline.h>
#include <readline/history.h>

#define ERR(...) err_msg(__func__, __VA_ARGS__)

static void err_msg(const char* func, const char* fmt, ...) {
    va_list va;
    va_start(va, fmt);

    fprintf(stderr, "%s: ", func);
    vfprintf(stderr, fmt, va);
    fprintf(stderr, "\n");

    va_end(va);
}

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

enum ETokenType {
    TOKEN_EOL, /* End Of List. Indicates the last item of Token.val.children */
    TOKEN_PARENT,
    TOKEN_OPERATOR,
    TOKEN_NUMLITERAL,
};

typedef struct Token {
    enum ETokenType type;
    union {
        int num;
        char ch;
        char* str;
        struct Token* children;
    } val;
} Token;

/* Allocate and initialize a new token */
static Token* token_new(void) {
    Token* t        = malloc(sizeof(Token));
    t->type         = TOKEN_EOL;
    t->val.children = NULL;
    return t;
}

static char* input   = NULL;
static int input_pos = 0;

/* Returns the character at input[input_pos + n] */
static inline char peek(int n) {
    return input[input_pos + n];
}

/* Returns the character at input[input_pos + n], and jumps to that position */
static inline char consume(int n) {
    input_pos += n;
    return input[input_pos];
}

/* Counts tokens from current position in input. Asumes input is pointing to
 * '(', and reads until ')' */
static int token_count(void) {
    char c = 0, prev = 0;
    int ret = 0;

    for (int i = 0; (c = peek(i)) != ')'; i++) {
        if (c == '\0') {
        unexpected_eof:
            ERR("Reached end of input. Expected ')'");
            break;
        }

        /* Previous char was a token separator */
        if (prev == '(' || prev == ' ') {
            /* Current token is not a separator */
            if (c != ' ')
                ret++;

            /* If it's the start of another list, skip it since it's only 1
             * parent token and we don't care about the children for now */
            if (c == '(') {
                i++;

                char tmp;
                while ((tmp = peek(i)) != ')')
                    if (tmp == '\0')
                        goto unexpected_eof;
                    else
                        i++;
            }
        }

        prev = c;
    }

    return ret;
}

/* Parses the input, and stores Tokens */
static void parse_token(Token* current) {
    char c;
    while ((c = consume(1)) != '\0') {
        if (c == '(') {
            /* Count tokens in current list */
            int child_num = token_count();

            /* Allocate children (+ 1 for EOL terminator) */
            current->type         = TOKEN_PARENT;
            current->val.children = malloc((child_num + 1) * sizeof(Token));

            /* End Of List terminator */
            current->val.children[child_num].type = TOKEN_EOL;

            /* TODO */
            for (int i = 0; i < child_num; i++)
                parse_token(&current->val.children[i]);
        }

        /* TODO */
    }
}

/* Returns the root of the Token tree. Must be free'd */
static Token* parse(char* in) {
    input     = in;
    input_pos = 0;

    Token* root = token_new();

    parse_token(root);

    return root;
}

/* Recursively Free all tokens from a tree */
static void free_tree(Token* parent) {
    /* If the token is a parent, free the children until End Of List */
    if (parent->type == TOKEN_PARENT)
        for (int i = 0; parent->val.children[i].type != TOKEN_EOL; i++)
            free_tree(&parent->val.children[i]);

    free(parent);
}

/*----------------------------------------------------------------------------*/

int main(void) {
    puts("--- Polish notation ---");

    bool quit = false;
    while (!quit) {
        char* user_input = readline("Input> ");

        /* The readline() function detected EOF with an empty line */
        if (user_input == NULL)
            break;

        add_history(user_input);

        if (!strcmp(user_input, "quit")) {
            quit = true;
            goto exit_loop;
        }

        /* FIXME */
        input     = user_input;
        input_pos = 0;

        int tokens = token_count();
        printf("Count: %d\n", tokens);

    exit_loop:
        /* Free allocated memory from readline() */
        free(input);
    }

    return 0;
}
