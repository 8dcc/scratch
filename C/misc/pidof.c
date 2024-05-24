/*
 * Get the PID of the first process that matches `process_name'.
 *
 * This code was first used in my signature scanning library, in the
 * "external-scanning" branch.
 *   https://github.com/8dcc/libsigscan
 */

#include <stdio.h>  /* fopen(), FILE* */
#include <string.h> /* strstr */
#include <stdlib.h> /* atoi() */
#include <dirent.h> /* readdir() */

static int my_pidof(const char* process_name) {
    static char filename[50];
    static char cmdline[256];

    DIR* dir = opendir("/proc");
    if (dir == NULL)
        return -1;

    struct dirent* de;
    while ((de = readdir(dir)) != NULL) {
        /* The name of each folder inside /proc/ is a PID */
        int pid = atoi(de->d_name);
        if (pid <= 0)
            continue;

        /* See proc_cmdline(5). You can also try:
         *   cat /proc/self/maps | xxd   */
        sprintf(filename, "/proc/%d/cmdline", pid);

        FILE* fd = fopen(filename, "r");
        if (fd == NULL)
            continue;

        char* fgets_ret = fgets(cmdline, sizeof(cmdline), fd);
        fclose(fd);

        if (fgets_ret == NULL)
            continue;

        /* We found the PID */
        if (strstr(cmdline, process_name)) {
            closedir(dir);
            return pid;
        }
    }

    /* We checked all /proc/.../cmdline's and we didn't find the process */
    closedir(dir);
    return -1;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <name>\n", argv[0]);
        return 1;
    }

    const char* name = argv[1];

    printf("pidof(\"%s\"): %d\n", name, my_pidof(name));
    return 0;
}
