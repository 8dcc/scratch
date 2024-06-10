
#include <stdio.h>
#include <setjmp.h>

int main(void) {
    int result;
    jmp_buf env_buffer;

    /* Save the current point in memory in `env_buffer' */
    result = setjmp(env_buffer);

    /* First call to setjmp() always returns zero */
    if (result != 0) {
        printf("Conditional got value: %d\n", result);
        return 0;
    }

    /* Jump back to the point where we called `setjmp', set `val' to 123 */
    printf("Calling `longjmp'...\n");
    longjmp(env_buffer, 123);

    return 0;
}
