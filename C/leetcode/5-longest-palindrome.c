
#include <stdio.h>
#include <string.h>

/*
 * https://leetcode.com/problems/longest-substring-without-repeating-characters/
 *
 * 5. Longest Palindromic Substring
 * ================================
 *
 * Given a string `s', return the longest palindromic substring in `s'.
 *
 * A string is palindromic if it reads the same forward and backward.
 *
 * Example
 * -------
 *
 * - Input: s = "babad"
 * - Output: "bab"
 * - Explanation: "aba" is also a valid answer.
 */

char* longestPalindrome(char* s) {
    const int sz = strlen(s);

    /* Start and end indexes of the palindrome, and its length */
    int best_l   = 0;
    int best_r   = 0;
    int best_len = 0;

    for (int i = 0; i < sz; i++) {
        /* Start looking for odd-sized palindromes, using `i' as the middle.
         * Keep expanding left and right pointers while they match. */
        for (int l = i, r = i, len = 1; l >= 0 && r < sz && s[l] == s[r];
             l--, r++, len += 2) {
            if (best_len < len) {
                best_len = len;
                best_r   = r;
                best_l   = l;
            }
        }

        /* Now do the same, but for even-sized palindromes. */
        for (int l = i, r = i + 1, len = 2; l >= 0 && r < sz && s[l] == s[r];
             l--, r++, len += 2) {
            if (best_len < len) {
                best_len = len;
                best_r   = r;
                best_l   = l;
            }
        }
    }

    /* We could allocate and copy, but let's take advantage of `s' not being
     * const. */
    s[best_r + 1] = '\0';
    return &s[best_l];
}

int main(void) {
    char case1[] = "babad";
    printf("Result 1: '%s'\n", longestPalindrome(case1));

    char case2[] = "cbbd";
    printf("Result 2: '%s'\n", longestPalindrome(case2));

    return 0;
}
