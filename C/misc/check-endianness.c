
#include <stdbool.h>
#include <stdio.h>

/* Check system endianness at run-time */
bool is_little_endian(void) {
    int n = 1;
    return *((char*)&n) == 1;
}

int main(void) {
    if (is_little_endian())
        puts("Little endian.");
    else
        puts("Big endian.");

    return 0;
}
