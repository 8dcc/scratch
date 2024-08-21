
#include <stdio.h>

/*
 * https://leetcode.com/problems/longest-substring-without-repeating-characters/
 *
 * 3. Longest Substring Without Repeating Characters
 * =================================================
 *
 * Given a string `s', find the length of the longest substring without
 * repeating characters.
 *
 * Example
 * -------
 *
 * - Input: s = "pwwkew"
 * - Output: 3
 * - Explanation: The answer is "wke", with the length of 3. Notice that the
 *   answer must be a substring, "pwke" is a subsequence and not a substring.
 */

int lengthOfLongestSubstring(char* s) {
    int result = 0;

    /* Index inside `s' where the substring starts */
    int substring_start = 0;

    for (int i = 0; s[i] != '\0'; i++) {
        /* Starting from the end, check if the substring contains the current
         * character in `s[i]'. If so, save the index of the right-most
         * occurrence. */
        int match_idx = -1;
        for (int j = i - 1; j >= substring_start; j--) {
            /* There was a match */
            if (s[j] == s[i]) {
                match_idx = j;
                break;
            }
        }

        /* There was a match in the substring. Set the next character as the
         * substring start. */
        if (match_idx >= 0)
            substring_start = match_idx + 1;

        /* Check if the new substring length is bigger than the previously
         * stored. */
        const int substring_len = i - substring_start + 1;
        if (result < substring_len)
            result = substring_len;
    }

    return result;
}

int main(void) {
    char* case1 = "abcabcbb";
    printf("Result 1: %d\n", lengthOfLongestSubstring(case1));

    char* case2 = "bbbbb";
    printf("Result 2: %d\n", lengthOfLongestSubstring(case2));

    char* case3 = "pwwkew";
    printf("Result 3: %d\n", lengthOfLongestSubstring(case3));

    return 0;
}
