#include <stdio.h>

int ten_times(int num);
int call_int_arg(int (*pFunc)(int), int arg);

int main() {
    int n1 = 123;

    printf("%d -> ", n1);
    n1 = call_int_arg(&ten_times, n1);
    printf("%d\n", n1);
}

/*
 * We use ( ) here:
 *   return_type(*ptr_name)(arg_type, ...)
 *              ^^^^^^^^^^^
 * Because if we didn't put them, we would be declaring a function called ptr_name,
 * that returns a 'return_type' pointer:
 *   return_type* ptr_name(arg_type, ...)
 * Instead of taking a pointer to a function that returns a 'return_type'
 */
int call_int_arg(int (*pFunc)(int), int arg) {
    // Just to make sure call_int_arg is doing something.
    arg *= 2;

    // In this case, returning pFunc(arg) is the same as (*pFunc)(arg), but I am not
    // sure what's the difference.
    return (*pFunc)(arg);
}

int ten_times(int num) {
    return num * 10;
}
