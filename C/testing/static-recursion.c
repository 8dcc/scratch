
#include <stdio.h>

/* Fill `out' with a list of digits from 0..N; Doesn't use indexing, and returns
 * the written digits. Caller must null-terminate the string. */
static int normal_recursion(char* out, int num) {
    /* Base case. */
    if (num < 0)
        return 0;

    /* Will start filling from end to start. Asumes `out' is big enough to hold
     * the digits. */
    out[num] = (num % 10) + '0';

    /* Call recursively, subtracting an index */
    return normal_recursion(out, num - 1) + 1;
}

/* Fill `out' with a list of digits from 0..N; Uses a static variable for
 * indexing. */
static char* static_recursion(char* out, int num) {
    /* Position in array, saved across recursive calls */
    static int out_pos = 0;

    /* Get digit of current position, save it in array */
    out[out_pos] = (out_pos % 10) + '0';

    /* We need to increase in a different statement since we used `out_pos' for
     * assigning and indexing. */
    out_pos++;

    /* If we haven't assigned `num' digits, call recursively */
    if (out_pos <= num)
        return static_recursion(out, num);

    /* We are done, terminate the `out' string */
    out[out_pos] = '\0';

    /* Also reset static variable for next recursive calls */
    out_pos = 0;

    /* Return the array */
    return out;
}

int main() {
    char s[20];
    int result;

    /* Non-static recursion, we must terminate the string */
    result    = normal_recursion(s, 5);
    s[result] = '\0';
    printf("f1(5):  %s (result=%d)\n", s, result);

    result    = normal_recursion(s, 13);
    s[result] = '\0';
    printf("f1(13): %s (result=%d)\n", s, result);

    /* Static recursion */
    printf("f2(5):  %s\n", static_recursion(s, 5));
    printf("f2(13): %s\n", static_recursion(s, 6));

    return 0;
}
