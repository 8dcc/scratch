
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "parser.h"
#include "util.h"

static char* input   = NULL;
static int input_pos = 0;

/*----------------------------------------------------------------------------*/

/* Returns the character at input[input_pos + n] */
static inline char peek(int n) {
    return input[input_pos + n];
}

/* Returns the character at input[input_pos + n], and jumps to that position */
static inline char consume(int n) {
    input_pos += n;
    return input[input_pos];
}

/* Allocate and initialize a new token */
static Token* token_new(void) {
    Token* t        = malloc(sizeof(Token));
    t->type         = TOKEN_EOL;
    t->val.children = NULL;
    return t;
}

/* FIXME: Make static */
/* Counts tokens in a list from current position in input. Asumes input is
 * pointing to '(', and reads until ')' */
int token_count(void) {
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

void input_init(char* in) {
    input     = in;
    input_pos = 0;
}

/* Returns the root of the Token tree. Must be free'd */
Token* parse(char* in) {
    input_init(in);

    Token* root = token_new();

    parse_token(root);

    return root;
}

/* Recursively Free all tokens from a tree */
void free_tree(Token* parent) {
    /* If the token is a parent, free the children until End Of List */
    if (parent->type == TOKEN_PARENT)
        for (int i = 0; parent->val.children[i].type != TOKEN_EOL; i++)
            free_tree(&parent->val.children[i]);

    free(parent);
}
