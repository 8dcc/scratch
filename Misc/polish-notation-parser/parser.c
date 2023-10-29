
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "parser.h"
#include "util.h"

/* Used for tree_print() */
#define INDENT_STEP 4

/*----------------------------------------------------------------------------*/

/* Allocate and initialize a new token */
static Token* token_new(void) {
    Token* t        = malloc(sizeof(Token));
    t->type         = TOKEN_PARENT;
    t->val.children = NULL;
    return t;
}

/* Counts tokens in a list from input. Asumes input is pointing to '(', and
 * reads until ')' */
static int token_count(char* in) {
    int ret = 0;

    for (int i = 0; in[i] != ')'; i++) {
        if (in[i] == '\0') {
        unexpected_eof:
            ERR("Reached end of input. Expected ')'.");
            break;
        }

        /* Previous char was a token separator */
        if (i > 0 && (in[i - 1] == '(' || isspace(in[i - 1]))) {
            /* Current token is not a separator */
            if (!isspace(in[i]))
                ret++;

            /* If it's the start of another list, skip it since it's only 1
             * parent token and we don't care about the children for now */
            if (in[i] == '(') {
                i++;

                while (in[i] != ')')
                    if (in[i] == '\0')
                        goto unexpected_eof;
                    else
                        i++;
            }
        }
    }

    return ret;
}

/* Used by token_store. Does not check '(' */
static inline bool is_token_separator(char c) {
    return isspace(c) || c == '\0' || c == ')';
}

/* Reads input until whitespace, and fills `out` token. For lists, use
 * parse_list() */
static char* token_store(Token* out, char* in) {
    bool only_digits = true;

    /* Store in `i` where the token ends. Also check if it's a number */
    int i;
    for (i = 0; !is_token_separator(in[i]); i++)
        if (!isdigit(in[i]))
            only_digits = false;

    if (only_digits) {
        out->type = TOKEN_NUM;

        /* Temporarily terminate string on space for atoi */
        char tmp     = in[i];
        in[i]        = '\0';
        out->val.num = atoi(in);
        in[i]        = tmp;
    } else {
        out->type    = TOKEN_OPERATOR;
        out->val.str = malloc(i + 1);

        if (out->val.str == NULL) {
            ERR("Error allocating token string. Aborting...");
            exit(1);
        }

        strncpy(out->val.str, in, i);
        out->val.str[i] = '\0';
    }

    return &in[i];
}

/* Parse the input, store Tokens. The input should point to '(' and will
 * recursivelly parse until ')'. It will return a pointer to the corresponding
 * '('. The children token array should be already allocated by caller. */
static char* parse_list(Token* parent, char* in) {
    if (*in == '(')
        in++;
    else
        ERR("Expected '(', ignoring.");

    /* Current token position inside parent->val.children[] */
    int child_i = 0;

    bool in_list = true;
    while (in_list) {
        switch (*in) {
            case '\0':
                ERR("Reached end of input. Expected ')'.");
                /* fallthrough */
            case ')':
                in_list = false;
                break;
            case '(': {
                /* Count tokens of child list */
                int child_count = token_count(in);

                /* Current child of `parent`, will be used as parent for the
                 * sublist. Extra variable for readability */
                Token* const cur = &parent->val.children[child_i];

                /* Allocate children (+ 1 for EOL terminator) */
                cur->type         = TOKEN_PARENT;
                cur->val.children = malloc((child_count + 1) * sizeof(Token));

                /* End Of List terminator (for the sublist) */
                cur->val.children[child_count].type = TOKEN_EOL;

                /* Parse sublist, update input pointer */
                in = parse_list(cur, in);

                /* Skip char since it parse_list returned pointer to ')' */
                in++;

                /* Go to the next child Token */
                child_i++;
                break;
            }
            case ' ': /* isspace() */
            case '\f':
            case '\n':
            case '\r':
            case '\t':
            case '\v':
                in++;
                break;
            default: {
                /* Current child of `parent`. Extra variable for readability */
                Token* const cur = &parent->val.children[child_i];

                /* Store token from input, update pointer */
                in = token_store(cur, in);

                /* Go to the next child Token */
                child_i++;
                break;
            }
        }
    }

    return in;
}

/* Returns the root of the Token tree. Must be freed with tree_free() */
Token* parse(char* in) {
    Token* root = token_new();

    /* Count child tokens in first level */
    int child_count = token_count(in);

    /* Allocate tokens for first level + End Of List terminator */
    root->val.children = malloc((child_count + 1) * sizeof(Token));
    root->val.children[child_count].type = TOKEN_EOL;

    /* Parse first level and children */
    parse_list(root, in);

    return root;
}

static inline void print_indent(int indent) {
    if (indent == 0)
        return;

    for (int i = 0; i < indent; i++)
        putchar(' ');
}

/* Recursively print all Tokens from a tree */
void tree_print(Token* parent, int indent) {
    switch (parent->type) {
        case TOKEN_PARENT:
            print_indent(indent);
            printf("[LIST]\n");

            /* If the token is a parent, print the children until End Of List */
            for (int i = 0; parent->val.children[i].type != TOKEN_EOL; i++)
                tree_print(&parent->val.children[i], indent + INDENT_STEP);
            break;
        case TOKEN_OPERATOR:
            print_indent(indent);
            printf("[OP] \"%s\"\n", parent->val.str);
            break;
        case TOKEN_NUM:
            print_indent(indent);
            printf("[NUM] %d\n", parent->val.num);
            break;
        case TOKEN_EOL:
        default:
            break;
    }
}

/* Recursively free all Tokens from a tree */
void tree_free(Token* current) {
    switch (current->type) {
        case TOKEN_PARENT:
            /* If the token is a parent, free the children until End Of List */
            for (int i = 0; current->val.children[i].type != TOKEN_EOL; i++)
                tree_free(&current->val.children[i]);
            break;
        case TOKEN_OPERATOR:
            /* If the token is an operator, free the allocated string before
             * freeing the Token */
            free(current->val.str);
            break;
        default:
            break;
    }

    free(current);
}
