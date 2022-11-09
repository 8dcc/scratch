/* 5-04 */

#include <stdio.h>

int strend(char* s1, char* s2);

int main() {
    char str1[] = "I hate furries.";
    char str2[] = "ries.";    // Good
    char str3[] = "es.";      // Good
    char str4[] = "";         // Good
    char str5[] = "test";     // Bad

    if (strend(str1, str2))
        printf("\"%s\" ends with \"%s\"\n", str1, str2);
    else
        printf("\"%s\" does not end with \"%s\"\n", str1, str2);

    if (strend(str1, str3))
        printf("\"%s\" ends with \"%s\"\n", str1, str3);
    else
        printf("\"%s\" does not end with \"%s\"\n", str1, str3);

    if (strend(str1, str4))
        printf("\"%s\" ends with \"%s\"\n", str1, str4);
    else
        printf("\"%s\" does not end with \"%s\"\n", str1, str4);

    if (strend(str1, str5))
        printf("\"%s\" ends with \"%s\"\n", str1, str5);
    else
        printf("\"%s\" does not end with \"%s\"\n", str1, str5);

    return 0;
}

/* strend: checks if s1 ends with s2 using pointers */
int strend(char* s1, char* s2) {
    char* old_s2 = s2;

    // Move both ptrs to the end. Check 5-03 comment.
    while (*s1 != '\0') s1++;
    while (*s2 != '\0') s2++;

    // Decrease pointer until we have checked the whole s2 string.
    while (s2 >= old_s2) {
        if (*s1-- != *s2--) return 0;
    }

    return 1;
}
