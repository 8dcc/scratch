
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* For mkfifo() */
#include <sys/types.h>
#include <sys/stat.h>

/* O_RDONLY, O_NONBLOCK, etc. */
#include <fcntl.h>

#define PIPE_NAME "./test-pipe"

/* Buffer for reading data from pipe */
static char buf[100];

int main(void) {
    /* 0666 -> prw-rw-rw- */
    if (mkfifo(PIPE_NAME, 0666) != 0)
        printf("Note: File '" PIPE_NAME "' already exists.\n");

    /* Open the pipe we just created as read-only */
    int fd = open(PIPE_NAME, O_RDONLY | O_NONBLOCK);
    if (fd < 0) {
        fprintf(stderr, "Error: Failed to open '" PIPE_NAME "'");
        return 1;
    }

    /* Main loop for reading data from the pipe */
    for (;;) {
        int read_bytes = read(fd, buf, sizeof(buf));
        if (read_bytes <= 0)
            continue;

        printf("[%d] %s\n", read_bytes, buf);
    }

    return 0;
}
