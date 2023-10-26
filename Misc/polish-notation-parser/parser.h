#ifndef PARSER_H_
#define PARSER_H_

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

/*----------------------------------------------------------------------------*/

/* FIXME */
int token_count(void);

void input_init(char* in);
Token* parse(char* in);
void free_tree(Token* parent);

#endif /* PARSER_H_ */
