/* 5-03 */

#include <stdio.h>

#define BUFSIZE 100

int strcat_ptr(char* dest, char* src);

int main() {
    char s1[BUFSIZE] = "Hello, ";
    char s2[BUFSIZE] = "world!";

    int diff = strcat_ptr(s1, s2);

    printf("%d: %s\n", diff, s1);

    return 0;
}

/*
 * strcat_ptr: Concatenates src to the end of dest using pointer arithmetic. Returns
 * final length of dest, including null char.
 */
int strcat_ptr(char* dest, char* src) {
    char* old = dest;

    // Could do *dest++, but we don't want to increment after we find '\0'
    while (*dest != '\0')
        dest++;

    while ((*dest++ = *src++) != '\0')
        ;

    *dest = '\0';

    return dest - old;
}
