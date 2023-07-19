/* See comments on libtest-for-libraries.c */

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

/* In here we just declare extern and change directly. The variable inside main
 * obviously can't be static */
extern int test_var;

__attribute__((constructor)) void load(void) {
    printf("Loaded.\n");
    test_var = 11;
}
