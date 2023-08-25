/* https://app.hackthebox.com/challenges/366 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    char buf[50] = { '\0' };
    char* ptr    = buf;

    char c;
    while ((c = getchar()) != EOF)
        *ptr++ = c;
    *ptr = EOF;

    /* main+0x15C -> main+0x170 */
    uint32_t seed = *(uint32_t*)buf;
    srand(seed);

    fprintf(stderr, "Seed: %d\n", seed);

#ifdef TEST_ROTATIONS
    srand(1337);

    char test0 = 'a';
    printf("%c -> ", test0);

    /* ROL(test0, r); */
    uint32_t r    = rand() & 7;
    uint8_t test1 = (test0 << r) | (test0 >> (8 - r));
    printf("%d -> ", test1);

    /* ROR(test1, r); */
    test0 = (test1 >> r) | (test1 << (8 - r));
    printf("%c\n", test0);

    srand(seed);
#endif

    /* Opposite code of encrypt.c */
    int i;
    for (i = 4; buf[i] != EOF; i++) {
        uint32_t r1 = rand();
        uint32_t r2 = rand() & 7;

        /* https://en.wikipedia.org/wiki/Circular_shift */
        uint8_t tmp = buf[i];
        buf[i]      = (tmp >> r2) | (tmp << (8 - r2));

        buf[i] ^= r1;
    }

    buf[i] = '\0';
    printf("%s\n", &buf[4]);

    return 0;
}
