/* Kata: https://www.codewars.com/kata/52774a314c2333f0a7000688 */

#include <stdbool.h>

bool validParentheses(const char *str_in) {
    int p_count_o = 0, p_count_c = 0;

    for (int n = 0; str_in[n] != '\0'; n++) {
        if (str_in[n] == '(') {
            p_count_o++;
        } else if (str_in[n] == ')') {
            if (p_count_o - p_count_c < 1) return false;    // Check if we try to close without an open one
            p_count_c++;
        }
    }

    return (p_count_o - p_count_c == 0);                    // Check if we left one without closing
}
