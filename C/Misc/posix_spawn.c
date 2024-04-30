/* See posix_spawn(3) */

#include <stdio.h>
#include <unistd.h>
#include <errno.h>    /* errno */
#include <spawn.h>    /* posix_spawn() */
#include <sys/wait.h> /* parent_waitpid() */

/* This symbol points to our enviroment variables. Will be passed to
 * posix_spawn(). */
extern char** environ;

int main(void) {
    /* Arguments for the program. The program expects the executable path in
     * argv[0]. Has to be NULL-terminated, see also exec(3) */
    char* const argv[] = { "./strxor.out", "Foo", NULL };

    /* NOTE: We could also declare our own enviroment variables. NULL-terminated
     * like argv[]. */
    char* const envp[] = { "FOO=bar", NULL };

    pid_t pid;
    if (posix_spawn(&pid, "./strxor.out", NULL, NULL, argv, environ) != 0) {
        fprintf(stderr, "Error: posix_spawn returned non-zero (%d).\n", errno);
        return 1;
    }

    /* Wait for child process to stop */
    int waitpid_status;
    waitpid(pid, &waitpid_status, 0);

    return 0;
}
