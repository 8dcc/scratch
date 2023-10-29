
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "calc.h"
#include "parser.h"
#include "util.h"

#define LENGTH(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

typedef int32_t (*FuncPtr)(int32_t a, int32_t b);

typedef struct {
    const char* name;
    FuncPtr fn;
} OperatorPair;

/*----------------------------------------------------------------------------*/

static int32_t sum(int32_t a, int32_t b) {
    return a + b;
}

static int32_t sub(int32_t a, int32_t b) {
    return a - b;
}

static int32_t mul(int32_t a, int32_t b) {
    return a * b;
}

static int32_t div(int32_t a, int32_t b) {
    if (b == 0) {
        ERR("Division by zero.");
        return 0;
    }

    return a / b;
}

static OperatorPair operators[] = {
    { "+", sum },
    { "-", sub },
    { "*", mul },
    { "/", div },
};

/*----------------------------------------------------------------------------*/

/* Calculate result from Token tree */
int32_t calc(Token* tree) {
    if (tree->type != TOKEN_PARENT) {
        ERR("Expected a parent token.");
        return 0;
    }

    if (tree->val.children[0].type != TOKEN_OPERATOR) {
        ERR("Expected an operator as the first element of list.");
        return 0;
    }

    FuncPtr fnOperation = NULL;
    for (size_t i = 0; i < LENGTH(operators); i++) {
        if (!strcmp(operators[i].name, tree->val.children[0].val.str)) {
            fnOperation = operators[i].fn;
            break;
        }
    }

    if (fnOperation == NULL) {
        ERR("The string \"%s\" is not in the operator list.",
            tree->val.children[0].val.str);
        return 0;
    }

    if (tree->val.children[1].type != TOKEN_NUM) {
        ERR("Expected a number as the second element of list.");
        return 0;
    }

    /* The initial value is the first number */
    int32_t total = tree->val.children[1].val.num;

    /* Iterate children and keep using the selected operation on them */
    for (int i = 2; tree->val.children[i].type != TOKEN_EOL; i++) {
        switch (tree->val.children[i].type) {
            case TOKEN_OPERATOR:
                ERR("Unexpected operator \"%s\" at position %d.",
                    tree->val.children[i].val.str, i);
                return 0;
            case TOKEN_NUM:
                total = fnOperation(total, tree->val.children[i].val.num);
                break;
            case TOKEN_PARENT:
                /* Recursivelly calc() children, and perform OP with total */
                total = fnOperation(total, calc(&tree->val.children[i]));
                break;
            case TOKEN_EOL:
            default:
                ERR("Unexpected token type (%d) at position %d.",
                    tree->val.children[i].type, i);
                return 0;
        }
    }

    return total;
}
