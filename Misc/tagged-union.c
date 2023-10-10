
#include <stdint.h>
#include <stdio.h>

#define PRINT_EXPR(E)               \
    do {                            \
        printf("%s: %ld\n", #E, E); \
    } while (0);

#define PRINT_INFO(T)                  \
    do {                               \
        printf("print_val(%s): ", #T); \
        print_val(T);                  \
        putchar('\n');                 \
    } while (0);

typedef struct {
    enum { STR, NUM, FLT } tag;
    union {
        const char* s;
        uint32_t n;
        float f;
    } val;
} TaggedUnion;

static void print_val(TaggedUnion* t) {
    switch (t->tag) {
        case STR:
            printf("%s", t->val.s);
            break;
        case NUM:
            printf("%d", t->val.n);
            break;
        case FLT:
            printf("%f", t->val.f);
            break;
        default:
            fprintf(stderr, "print_val: Invalid tag for TaggedUnion.\n");
            break;
    }
}

int main(void) {
    TaggedUnion t;

    /* NOTE: The size of TaggedUnion will be the size of int (tag) and the size
     * of the largest item in the union (val). When compiling in 64-bits, the
     * largest value of the union will be 8 (size of pointer), but in 32-bits it
     * will be 4 (all types will have the same size) */
    PRINT_EXPR(sizeof(int));
    PRINT_EXPR(sizeof(char*));
    PRINT_EXPR(sizeof(uint32_t));
    PRINT_EXPR(sizeof(float));
    putchar('\n');
    PRINT_EXPR(sizeof(TaggedUnion));
    putchar('\n');

    t.tag   = STR;
    t.val.s = "Hello, world!";
    PRINT_INFO(&t);

    t.tag   = NUM;
    t.val.n = 1337;
    PRINT_INFO(&t);

    t.tag   = FLT;
    t.val.f = 69.05;
    PRINT_INFO(&t);
}
