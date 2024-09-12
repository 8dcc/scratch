/*
 * Used for my Lisp interpreter: https://github.com/8dcc/sl
 */

#include <stdbool.h>
#include <stdio.h>

/*
 * Did the character at `str[pos]' just open/close a string with a double-quote?
 * Checks if it was escaped.
 *
 * The initial version just checked for two backslashes:
 *
 *   return str[pos] == '\"' && (pos < 1 || str[pos - 1] != '\\' ||
 *                               pos < 2 || str[pos - 2] == '\\');
 *
 * That version was not really complete, but is enough to illustrate the basic
 * behavior. We need to iterate `i' backwards and check if the number of
 * backslashes was odd or even:
 *
 *   (...\\\")  -> false (odd '\')
 *   (...\\\\") -> true (even '\')
 */
static bool just_toggled_string_state(const char* str, int pos) {
    if (str[pos] != '\"')
        return false;
    pos--;

    /* Every consecutive backslash from the end, toggle the variable to store if
     * the number is odd or even. */
    bool odd_backslashes = true;
    for (; pos >= 0 && str[pos] == '\\'; pos--)
        odd_backslashes = !odd_backslashes;

    return odd_backslashes;
}

static void print_boundaries(const char* s) {
    bool inside_string = false;
    for (int i = 0; s[i] != '\0'; i++) {
        /* The actual function we are trying to test */
        if (just_toggled_string_state(s, i)) {
            inside_string = !inside_string;
            putchar('|');
        } else {
            putchar(inside_string ? 's' : '.');
        }
    }
}

static void print_string_and_boundaries(const char* s) {
    printf("%s\n", s);
    print_boundaries(s);
    printf("\n\n");
}

int main(void) {
    print_string_and_boundaries("abc");
    print_string_and_boundaries("ab\"c");
    print_string_and_boundaries("ab\"cde\"fg");
    print_string_and_boundaries("ab\"cde\\\"fg");
    print_string_and_boundaries("ab\"cd\\\"e\"fg");
    return 0;
}
