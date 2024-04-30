/*
 * I might use this for reading /proc/PID/mem as a pointer instead of byte
 * stream.
 *
 * See also:
 * https://stackoverflow.com/questions/56418803/mapping-file-into-memory-and-writing-beyong-end-of-file
 */

#include <stdio.h>
#include <sys/mman.h> /* mmap() */
#include <errno.h>    /* errno */

#define FILE_PATH "/tmp/mmap_test.txt"
#define TEST_DATA "My test data"

int main(void) {
    /* Create test file */
    FILE* fp = fopen(FILE_PATH, "w");
    if (fp == NULL) {
        printf("Can't open file %s for writing\n", FILE_PATH);
        return 1;
    }

    fwrite(TEST_DATA, sizeof(char), sizeof(TEST_DATA), fp);
    fclose(fp);

    /* Re-open as read */
    fp = fopen(FILE_PATH, "r");
    if (fp == NULL) {
        printf("Can't open file %s for reading\n", FILE_PATH);
        return 1;
    }

    /* Map the first N bytes of `fp' at offset 0 to the returned pointer */
    char* mapped =
      mmap(NULL, sizeof(TEST_DATA), PROT_READ, MAP_SHARED, fileno(fp), 0);

    if (mapped == MAP_FAILED) {
        printf("mmap() returned MAP_FAILED. Errno: %d\n", errno);
        return 1;
    }

    for (size_t i = 0; i < sizeof(TEST_DATA); i++)
        printf("%02X | %c\n", mapped[i], mapped[i]);

    fclose(fp);
    return 0;
}
