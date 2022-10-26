/* 4-06 */

/*
 * For differences between 4-04 and 4-03 (commands and comments for example), run:
 *   diff 4-05* 4-06*
 *
 * Compile using "-lm" for linking the math lib:
 *   gcc -o reverse-polish-calc-vars.out 4-06-reverse-polish-calc-vars.c -lm
 */

#include <stdio.h>
#include <stdlib.h>    // atof()
#include <string.h>    // strncmp()
#include <math.h>      // fmod()
#include <ctype.h>     // isdigit(), isalpha()

#define MAXOP 100    // Max size of operand or operator

// Number (in this case 48) to return by getop() when we find numbers
#define NUMCODE '0'

// Number (in this case 49) to return by getop() when we find a cmd
#define CMDCODE '1'

// Number (in this case 50) to return by getop() when we find a 1 char variable
#define VARCODE '2'

int getop(char* buf);
void push(double n, int bVar);
double pop(int bIgnoreVars);
double get_var(char var);
void save_var(double val, char var);

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
                push(atof(s), 0);
                break;
            case VARCODE:
                // Push directly the variable char as double and with bVar as true.
                // See push() for more info.
                push((double)s[0], 1);
                break;
            case '+':
                // Pop last 2 numbers and add them. Then push. We pass the returns
                // directly because we don't care about the order when adding.
                push(pop(0) + pop(0), 0);
                break;
            case '*':
                push(pop(0) * pop(0), 0);
                break;
            case '-':
                // We pop the 2nd one first because we push in the right order so we
                // pop in reverse. See declaration of op2 for more info.
                op2 = pop(0);
                push(pop(0) - op2, 0);
                break;
            case '/':
                op2 = pop(0);
                if (op2 != 0.0)
                    push(pop(0) / op2, 0);
                else
                    fprintf(stderr, "error: zero divisor\n");
                break;
            case '%':
                op2 = pop(0);
                if (op2 != 0.0)
                    push(fmod(pop(0), op2), 0);
                else
                    fprintf(stderr, "error: zero divisor\n");
                break;
            case CMDCODE:
                if (strncmp(s, "qu", 2) == 0) {    // Quit
                    return 0;
                } else if (strncmp(s, "pr", 2) == 0) {    // Print
                    print_stack();
                } else if (strncmp(s, "du", 2) == 0) {    // Duplicate
                    duplicate_stack();
                } else if (strncmp(s, "sw", 2) == 0) {    // Swap
                    swap_stack();
                } else if (strncmp(s, "cl", 2) == 0) {    // Clear
                    clear_stack();
                } else if (strncmp(s, "var", 3) == 0) {    // Variables
                    // Save value to variables using "N var_c var" where N is the
                    // number to be saved and var_c is the variable character.
                    // See comment on pop() about why we use 1
                    op2 = pop(1);
                    save_var(pop(0), (char)op2);
                } else if (strncmp(s, "sin", 3) == 0) {    // sin()
                    push(sin(pop(0)), 0);
                } else if (strncmp(s, "exp", 3) == 0) {    // exp()
                    push(exp(pop(0)), 0);
                } else if (strncmp(s, "pow", 3) == 0) {    // pow()
                    op2 = pop(0);
                    push(pow(pop(0), op2), 0);
                } else {
                    fprintf(stderr, "error: unknown command \"%s\"\n", s);
                }
                break;
            case '\n':
                // Pop final result and print it
                printf("%g\n", pop(0));
                // Clear stuff for the next line
                clear_stack();
                break;
            default:
                fprintf(stderr, "error: unknown command \"%s\"\n", s);
                break;
        }
    }

    return 0;
}

/* ----------------------------------------------------------- */

#define MAXSTACK 100    // Size of operation and operator stack

double stack[MAXSTACK];
int sp = 0;    // Next free pos of the stack

/*
 * I would use a struct for the stack to check if something is a variable, but
 * structs are seen later in the book.
 * This arr will contain 1 in the stack positions that contain a char variable:
 *
 *     stack = [ 1.225, 97.0, 108.0, 97.0, 122.0 ]
 * var_stack = [     0,    1,     0,    0,     1 ]
 *     final = [ 1.225,  'a', 108.0, 97.0,   'z' ]
 *
 * Where variables 'a' and 'z' are replaced with the values in var_arr[]
 */
int var_stack[MAXSTACK] = { 0 };

#define VAR_LOW_LEN sizeof("abcdefghijklmnopqrstuvwxyz")
#define VAR_UPP_LEN sizeof("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
#define VAR_LEN     VAR_LOW_LEN + VAR_UPP_LEN

double var_arr[VAR_LEN] = { 0 };

void push(double f, int bVar) {
    var_stack[sp] = (bVar) ? 1 : 0;

    // If we have space left in the stack, push
    if (sp < MAXSTACK)
        stack[sp++] = f;
    else
        fprintf(stderr, "error: stack full, can't push %g\n", f);
}

/*
 * Pops the last value from stack. If bIgnoreVars is 0, it will pop the variable
 * values using get_var().
 * bIgnoreVars is needed so we don't save a variable at that variable's index:
 *   save_var() ---> pop(1) -x-> get_var()
 * Save char needs to pop the character name of the variable, but we dont want to get
 * the saved variable at that idx.
 */
double pop(int bIgnoreVars) {
    // If we have at least one value, decrease the last available pos (because we
    // are popping) and return that.
    if (sp > 0) {
        // If the current stack item is a variable, convert to char and pass it to
        // get_var()
        if (!bIgnoreVars && var_stack[sp - 1] == 1)
            return get_var((char)stack[--sp]);
        else
            return stack[--sp];
    } else {
        fprintf(stderr, "error: stack empty\n");
        return 0.0;
    }
}

double get_var(char var) {
    if (var >= 'a' && var <= 'z')
        return var_arr[var - 'a'];
    else if (var >= 'A' && var <= 'Z')
        return var_arr[VAR_LOW_LEN + var - 'A'];    // Start idx at VAR_MIN_LEN
    else
        return 0.0;
}

/*
 * Save var to the var array. Will get called when we use something like:
 *    1.513 a var
 * Will save 1.513 in the var array at the index corresponding to 'a'
 * It will pop both items (in main) without pushing anything.
 */
void save_var(double val, char var) {
    // See get_var()
    if (var >= 'a' && var <= 'z')
        var_arr[var - 'a'] = val;
    else if (var >= 'A' && var <= 'Z')
        var_arr[VAR_LOW_LEN + var - 'A'] = val;
    else
        fprintf(stderr, "error: variable '%c' out of range.\n", var);
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

/* Will append the current stack to the stack. Variables will remain as variables */
void duplicate_stack() {
    if (sp < 1) {
        fprintf(stderr, "error: can't duplicate empty stack\n");
        return;
    }

    const int o_sp = sp;    // Save old sp

    /*
     * Push each value from stack to the stack again. As the second argument (bVar)
     * we will simply pass the value of the var_stack array, so if the current stack
     * value is a variable, it will also be one when pushing.
     */
    for (int i = 0; i < o_sp; i++) push(stack[i], var_stack[i]);
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

    // Also swap variable values
    double var_buf    = var_stack[sp - 1];
    var_stack[sp - 1] = var_stack[sp - 2];
    var_stack[sp - 2] = var_buf;
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
        if (!isalpha(c)) return c;

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
        // We need to subtract 1 from j becuase the last char we got was an space,
        // and because we are using j++, it will increase the value after the space.
        s[--j] = '\0';

        // If j is 1 it means that s is:
        //   s = [ 'a', '\0' ]
        // So we interpret it as a variable instead of a command
        return (j == 1) ? VARCODE : CMDCODE;
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

