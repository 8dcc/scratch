/* 2-05 */

#include <stdio.h>

// Will return the position of the first character in s1 that is in also s2 (starting from 0)
int any(const char* s1, const char* s2) {
    for (int n = 0; s1[n] != '\0'; n++) {
        for (int i = 0; s2[i] != '\0'; i++) {
            if (s1[n] == s2[i]) return n;
        }
    }

    return -1;
}

int main() {
    const char* str1 = "ABCDEFG";
    const char* str2 = "FD";        // First appearance in str1 is 'D' (pos 3)

    int result = any(str1, str2);
    if (result == -1) printf("Not found.\n");
    else           printf("Found in pos: %d.\n", result);

    return 0;
}
