
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

/* Should change this to be absolute (and accurate) */
#define PATH "/home/username/c-stuff/Misc/injection/"

/*
 * We need:
 *   __attribute__((constructor))
 * To indicate that this function will be the entry point once injected.
 */
__attribute__((constructor)) void load(void) {
    printf("Loaded.\n");

    /*
     * afaik dlopen() doesn't work with executables, only with libraries:
     *   "Error loading library: /.../main.out: cannot dynamically load
     *   position-independent executable"
     */
    void* handle = dlopen(PATH "main.out", RTLD_NOW);
    if (!handle) {
        printf("Error loading library: %s\n", dlerror());
        exit(1);
    }

    /* Get symbol from the handle we just got, cast into type and change value
     * bellow */
    int* test_var_ptr = (int*)dlsym(handle, "test_var");
    printf("Got ptr.\n");

    *test_var_ptr = 11;
}
