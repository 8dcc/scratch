/* 5-05 */

#include <stdio.h>

#define BUFSIZE 100

char* strncpy_p(char* d, const char* s, int n);
char* strncat_p(char* d, const char* s, int n);
int strncmp_p(const char* d, const char* s, int n);

int main() {
    const char str1[100] = "I hate furries.";    // str1 should not change
    char str2[100]       = { 0 };                // For strncpy_p
    char str3[100]       = "I ha";               // For strncmp_p

    printf("%10s | \"%s\"\n", "Original", str1);

    strncpy_p(str2, str1, 9);
    printf("%10s | \"%s\"\n", "strncpy 9", str2);

    // s is 14 chars, only append 6
    strncat_p(str2, "rries. Period.", 6);
    printf("%10s | \"%s\"\n", "strncat 6", str2);

    // To show that it won't copy past s, even if n is larger
    strncat_p(str2, " Have a good day.", 99);
    printf("%10s | \"%s\"\n", "strncat 99", str2);

    if (strncmp_p(str1, str3, 3))
        printf("%10s | \"%s\" matches \"%s\"\n", "strncmp 3", str1, str3);
    else
        printf("%10s | \"%s\" does not match \"%s\"\n", "strncmp 3", str1, str3);

    if (strncmp_p(str1, str3, 8))
        printf("%10s | \"%s\" matches \"%s\"\n", "strncmp 8", str1, str3);
    else
        printf("%10s | \"%s\" does not match \"%s\"\n", "strncmp 8", str1, str3);

    return 0;
}

/* strncpy: Copies n chars of s to the begining of d (overwriting). Returns dest */
char* strncpy_p(char* d, const char* s, int n) {
    for (int i = 0; i < n && *s != '\0'; i++)
        *d++ = *s++;

    *++d = '\0';

    return d;
}

/* strncat: Appends n chars of s to the end of d. Returns dest */
char* strncat_p(char* d, const char* s, int n) {
    // Make d point to the last char
    while (*d != '\0')
        d++;

    for (int i = 0; i < n && *s != '\0'; i++)
        *d++ = *s++;

    *++d = '\0';

    return d;
}

/*
 * strncmp: Checks if the first n chars of s1 are the first n chars of s2.
 * Returns 1 if they match.
 */
int strncmp_p(const char* s1, const char* s2, int n) {
    // Iterate n times. If a char does not match or any of the 2 strs are not long
    // enough, strncmp returns false.
    for (int i = 0; i < n; i++)
        if (*s1++ != *s2++ || *s1 == '\0' || *s2 == '\0')
            return 0;

    return 1;
}
