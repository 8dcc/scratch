/* 4-04 */

/*
 * For differences between 4-04 and 4-03 (commands and comments for example), run:
 *   diff 4-04* 4-05*
 *
 * Compile using "-lm" for linking the math lib:
 *   gcc -o reverse-polish-calc-cmd.out 4-05-reverse-polish-calc-cmd.c -lm
 */

#include <stdio.h>
#include <stdlib.h>    // atof()
#include <string.h>    // strncmp()
#include <math.h>      // fmod()
#include <ctype.h>     // isdigit()

#define MAXOP 100    // Max size of operand or operator

// Number (in this case 48) to return by getop() when we find numbers
#define NUMCODE '0'

// Number (in this case 49) to return by getop() when we find a cmd
#define CMDCODE '1'

int getop(char* buf);
void push(double n);
double pop();

static inline void print_stack();
static inline void duplicate_stack();
static inline void swap_stack();
static inline void clear_stack();

int main() {
    int op_type = 0;     // Used to store value returned by getop()
    double op2 = 0.0;    // Used to store operators when the order is important (like
                         // a subtraction or division)
    char s[MAXOP];       // Operand buffer. Used to store numbers for getop()

    // Save return op_type for the switch
    while ((op_type = getop(s)) != EOF) {
        switch (op_type) {
            // Push operands
            case NUMCODE:
                push(atof(s));
                break;
            case '+':
                // Pop last 2 numbers and add them. Then push. We pass the returns
                // directly because we don't care about the order when adding.
                push(pop() + pop());
                break;
            case '*':
                push(pop() * pop());
                break;
            case '-':
                // We pop the 2nd one first because we push in the right order so we
                // pop in reverse. See declaration of op2 for more info.
                op2 = pop();
                push(pop() - op2);
                break;
            case '/':
                op2 = pop();
                if (op2 != 0.0)
                    push(pop() / op2);
                else
                    fprintf(stderr, "error: zero divisor\n");
                break;
            case '%':
                op2 = pop();
                if (op2 != 0.0)
                    push(fmod(pop(), op2));
                else
                    fprintf(stderr, "error: zero divisor\n");
                break;
            case CMDCODE:
                if (strncmp(s, "qu", 2) == 0) {
                    return 0;
                } else if (strncmp(s, "pr", 2) == 0) {
                    print_stack();
                } else if (strncmp(s, "du", 2) == 0) {
                    duplicate_stack();
                } else if (strncmp(s, "sw", 2) == 0) {
                    swap_stack();
                } else if (strncmp(s, "cl", 2) == 0) {
                    clear_stack();
                } else {
                    fprintf(stderr, "error: unknown command %s\n", s);
                }
                break;
            case '\n':
                // Pop final result and print it
                printf("%g\n", pop());
                // Clear stuff for the next line
                clear_stack();
                break;
            default:
                fprintf(stderr, "error: unknown command %s\n", s);
                break;
        }
    }

    return 0;
}

/* ----------------------------------------------------------- */

#define MAXSTACK 100    // Size of operation and operator stack

double stack[MAXSTACK];
int sp = 0;    // Next free pos of the stack

void push(double f) {
    // If we have space left in the stack, push
    if (sp < MAXSTACK)
        stack[sp++] = f;
    else
        fprintf(stderr, "error: stack full, can't push %g\n", f);
}

double pop() {
    // If we have at least one value, decrease the last available pos (because we
    // are popping) and return that.
    if (sp > 0) {
        return stack[--sp];
    } else {
        fprintf(stderr, "error: stack empty\n");
        return 0.0;
    }
}

void print_stack() {
    if (sp < 1) {
        fprintf(stderr, "stack: [ ] (empty)\n");
        return;
    }

    int i;

    printf("stack: [ ");

    // From first to penultimate so we don't print the last ','
    for (i = 0; i < sp - 1; i++) printf("%g, ", stack[i]);

    printf("%g ]\n", stack[i]);
}

void duplicate_stack() {
    if (sp < 1) {
        fprintf(stderr, "error: can't duplicate empty stack\n");
        return;
    }

    const int o_sp = sp;    // Save old sp

    for (int i = 0; i < o_sp; i++) push(stack[i]);
}

/* Swaps the last 2 elements of the stack */
void swap_stack() {
    if (sp < 2) {
        fprintf(stderr, "error: can't swap stack with less than 2 elements\n");
        return;
    }

    double val_buf = stack[sp - 1];
    stack[sp - 1]  = stack[sp - 2];
    stack[sp - 2]  = val_buf;
}

void clear_stack() {
    for (int i = 0; i < sp; i++) stack[i] = '\0';
    sp = 0;
}

/* ----------------------------------------------------------- */

int getch();
void ungetch(int c);

int getop(char* s) {
    // Used to write the cmd into s. i will be used when writing the number
    int j = 0;
    // Used to store the last char returned by getch()
    int c;

    // Get char and save it in c and s[0] until we enounter a non-whitespace char
    while ((s[j] = c = getch()) == ' ' || c == '\t')
        ;

    // Not a number
    if (!isdigit(c) && c != '.') {
        // Return non-cmd chars like operators or '\n'.
        if (tolower(c) < 'a' || tolower(c) > 'z') return c;

        // Increase the value of j to keep the char that we got after the spaces
        j++;

        // Save chars into s until we encounter a space or newline. We only save it
        // in c for the next if.
        while (!isspace(s[j++] = c = getch()))
            ;

        // If the last char is newline, unget it because we will need to return it
        // for printing the results.
        if (c == '\n') ungetch(c);

        // We add null terminator to s, so if we got "quit", we can return
        // CMD_CODE to main (let it know we got a cmd), and s will be:
        //   s = [ 'q', 'u', 'i', 't', '\0' ].
        s[j] = '\0';

        return CMDCODE;
    }

    // Save all digits in s starting at idx 1, and the last char in c
    int i = 0;
    if (isdigit(c))
        while (isdigit(s[++i] = c = getch()))
            ;

    /*
     * If the last char we got is a dot, keep adding to the string (s). We don't
     * need to worry about decimal powers like in 4-02, since we will pass all
     * this string to atof() in main if we return NUMCODE (to let main know that
     * s contains a number).
     */
    if (c == '.')
        while (isdigit(s[++i] = c = getch()))
            ;

    // We got all the number
    s[i] = '\0';

    // If the char is not EOF, pass it to ungetch(), so next time getch() is
    // called, it gets that instead of stdin.
    if (c != EOF) ungetch(c);

    // Let main know s contains a number, and that it should run atof()
    return NUMCODE;
}

/* ----------------------------------------------------------- */

#define BUFSIZE 100

char ch_buf[BUFSIZE];    // Buffer used by getch and ungetch
int ch_buf_p = 0;        // ch_buff pos

int getch() {
    // If we have a char in the buffer, return it, if not return from stdin.
    // See comment in pop() for more details.
    return (ch_buf_p > 0) ? ch_buf[--ch_buf_p] : getchar();
}

void ungetch(int c) {
    if (ch_buf_p >= BUFSIZE)
        fprintf(stderr, "ungetch: too many characters\n");
    else
        ch_buf[ch_buf_p++] = c;
}

