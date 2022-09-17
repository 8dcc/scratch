/* Program for testing the stack with function arguments */

#include <stdio.h>

int main(int argc, char** argv) {
    int a = argc;
    const char* test = (argc > 0) ? argv[1] : 0x0;

    printf("Args: %d | Arg1: %s\n", a, test);

    return 0;
}
