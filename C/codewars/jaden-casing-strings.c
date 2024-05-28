/* Kata: https://www.codewars.com/kata/5390bac347d09b7da40006f6 */
/* Kinda weird kata but ok */

#include <string.h>

char upper(const char);

char *to_jaden_case (char *jc, const char *njc)
{
    strcpy(jc, njc);                                // Clone the original string to change only whats necesary
    jc[0] = upper(jc[0]);                           // First letter
    for (int n = 0; jc[n] != '\0'; n++) {
        if (jc[n-1] == ' ') jc[n] = upper(jc[n]);   // If its the first char of a word, replace with upper version
    }
    return jc;
}

char upper(const char c) {
    // If its lowercase, return itslelf + upper offset. Else, return itself.
    if (c >= 'a' && c <= 'z') return c - ('a' - 'A');
    return c;
}
