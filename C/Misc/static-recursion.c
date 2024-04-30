#include <stdio.h>

char* static_recursion(char* arr, int num);

int main() {
    char s1[20] = { 0 };
    char s2[20] = { 0 };

    static_recursion(s1, 5);
    static_recursion(s2, 6);

    printf("s1 (5): %s\n"
           "s2 (6): %s\n",
           s1, s2);

    return 0;
}

// Fills arr with '0' to num, using recursion
char* static_recursion(char* arr, int num) {
    static int ap = 0;    // arr pos

    arr[ap] = ap + '0';
    ap++;    // New line to fix "-Wsequence-point". This is explained in K&R (See end
             // of 2.12)

    // After we are done with all recursions, reset static for next call
    if (ap > num)
        ap = 0;
    else
        static_recursion(arr, num);

    return arr;
}
