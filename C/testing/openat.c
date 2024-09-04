/* https://stackoverflow.com/a/78950612/11715554 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <unistd.h> /* close() */
#include <libgen.h> /* dirname() */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h> /* openat() */

FILE* get_included_file(const char* src, const char* rel_path) {
    /* We need to duplicate the source because `dirname' modifies it */
    char* src_copy     = strdup(src);
    char* src_dir_name = dirname(src_copy);

    int src_dir_fd = open(src_dir_name, O_RDONLY | O_DIRECTORY);
    if (src_dir_fd < 0) {
        fprintf(stderr, "Error opening directory '%s': %s", src_dir_name,
                strerror(errno));
        return NULL;
    }

    free(src_copy);

    int target_fd = openat(src_dir_fd, rel_path, O_RDONLY);
    if (target_fd < 0) {
        fprintf(stderr, "Error opening file '%s' included from '%s': %s",
                rel_path, src, strerror(errno));
        return NULL;
    }

    close(src_dir_fd);

    return fdopen(target_fd, "r");
}

int main(void) {
    const char* source       = "../misc/bf2nasm/bf2nasm.c";
    const char* include_path = "../../testing/openat.c";

    FILE* included = get_included_file(source, include_path);
    if (included == NULL) {
        fprintf(stderr, "Couldn't include file.\n");
        exit(1);
    }

    /* Print some sample bytes */
    int c;
    for (int i = 0; i < 52 && (c = fgetc(included)) != EOF; i++)
        putchar(c);

    return 0;
}
