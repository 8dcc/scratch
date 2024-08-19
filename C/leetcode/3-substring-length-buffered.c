/*
 * NOTE: For the original code and comments, see: 3-substring-length.c
 */

#include <stdbool.h>
#include <stdio.h>

int lengthOfLongestSubstring(char* s) {
    int result = 0;

    /* Contrary to my original code, this version doesn't re-iterate part of the
     * input for checking occurrences. Instead, it stores the index of the last
     * occurrence of each character. This version allocates more stack space,
     * but is faster. */
    int last_occurrences[256];
    for (int i = 0; i < 256; i++)
        last_occurrences[i] = -1;

    /* Index inside `s' where the substring starts */
    int substring_start = 0;

    for (int i = 0; s[i] != '\0'; i++) {
        /* For readability */
        const unsigned char cur_char = s[i];

        /* The last occurrence of the current character is in the substring. Set
         * the next character as the substring start. */
        if (last_occurrences[cur_char] >= substring_start)
            substring_start = last_occurrences[cur_char] + 1;

        /* Check if the new substring length is bigger than the previously
         * stored. */
        const int substring_len = i - substring_start + 1;
        if (result < substring_len)
            result = substring_len;

        /* Store the current index as the last appearance of the current
         * character, for the next iterations. */
        last_occurrences[cur_char] = i;
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
