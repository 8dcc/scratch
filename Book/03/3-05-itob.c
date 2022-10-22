/* 3-05 */

#include <stdio.h>
#include <string.h>    // strlen(), memset()
#include <stdlib.h>    // calloc()

#define LENGTH(s) sizeof(s) / sizeof(s[0])

char* reverse(char* s);
char* itob(int n, int b, char* dest);

int main() {
    int num       = 1337;
    char str[255] = { 0 };

    memset(str, '\0', sizeof(str));
    itob(num, 2, str);
    printf("%d in base 2: %s\n", num, str);
    
    memset(str, '\0', sizeof(str));
    itob(num, 5, str);
    printf("%d in base 5: %s\n", num, str);

    memset(str, '\0', sizeof(str));
    itob(num, 10, str);
    printf("%d in base 10: %s\n", num, str);

    memset(str, '\0', sizeof(str));
    itob(num, 16, str);
    printf("%d in base 16: %s\n", num, str);

    return 0;
}

/*
 * Converts n to base b and saves it in dest as a string.
 * Max base is [0-9A-Z]
 */
char* itob(int n, int b, char* dest) {
    const char bases[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    if (b >= sizeof(bases)) {
        fprintf(stderr, "Base %d not valid.\n", b);
        return "";
    }

    // Current char in the dest string
    int dest_p = 0;

    // Division quotient and remainder. Start quotient as n because we will use that
    // when dividing.
    int div_q = n;
    int div_r = 0;

    while (div_q > 0) {
        div_r          = div_q % b;    // Get remainder from last division and base
        dest[dest_p++] = bases[div_r];    // b[8] will be '8' but b[15] will be 'F'
        div_q /= b;                       // Divide
    }

    dest[dest_p] = '\0';

    /*
     * Because dest_p starts at 0, but when getting the reminders we are getting
     * numbers from the right, we need to reverse the dest string.
     *
     *  1337  (n)
     *  <---
     *
     * "7331" (dest in base 10)
     *  --->
     */
    reverse(dest);

    return dest;
}

/* Reverse string in place using a buffer */
char* reverse(char* s) {
    const int s_len = strlen(s);
    char* buff      = calloc(s_len, sizeof(char));
    int buff_p      = 0;

    for (int i = s_len - 1; i >= 0; i--) buff[buff_p++] = s[i];

    strcpy(s, buff);

    free(buff);
    return s;
}
