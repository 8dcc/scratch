#ifndef PARSER_H_
#define PARSER_H_ 1

#include <stdint.h>

enum ETokenType {
    TOKEN_EOL, /* End Of List. Indicates the last item of Token.val.children */
    TOKEN_PARENT,
    TOKEN_OPERATOR,
    TOKEN_NUM,
};

typedef struct Token {
    enum ETokenType type;
    union {
        int32_t num;
        char* str;
        struct Token* children;
    } val;
} Token;

/*----------------------------------------------------------------------------*/

Token* parse(char* in);
void tree_print(Token* parent, int indent);
void tree_free(Token* parent);

#endif /* PARSER_H_ */
