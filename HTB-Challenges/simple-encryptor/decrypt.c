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
    uint32_t seed = 0;
    for (int i = 0; i < 4; i++) {
        seed <<= 8;
        seed += buf[i];
    }

    printf("Seed: %d\n", seed);

    srand(seed);

#ifdef TEST_ROTATIONS
    char test0 = 'a';
    printf("%c -> ", test0);

    /* ROL(test0, r); */
    int r = rand() & 7;
    uint8_t test1 = (test0 << r) | (test0 >> (8 - r));
    printf("%d -> ", test1);

    /* ROR(test1, r); */
    test0 = (test1 >> r) | (test1 << (8 - r));
    printf("%c\n", test0);
#endif

#ifdef ENCRYPT_CODE
    for (int i = 4; buf[i] != EOF; i++) {
        /* main+0xC7 -> main+0xCC */
        uint32_t r = rand();

        /* main+0xCF -> main+0xF2 */
        buf[i] ^= r;

        /* main+0xF4 -> main+0xFC */
        r = rand() & 7;

        /* main+0xFE -> main+0x10C */
        uint8_t tmp = buf[i];

        /* main+0x10F -> main+0x130 (optimized) */
        buf[i] = (tmp << r) | (tmp >> (8 - r));
    }
#endif

    /* Reverse code of the loop above */
    /* TODO: Should be fine but it doesn't quite work */
    for (int i = 4; buf[i] != EOF; i++) {
        uint32_t r1 = rand();
        uint32_t r2 = rand() & 7;

        /* https://en.wikipedia.org/wiki/Circular_shift */
        uint8_t tmp = buf[i];
        buf[i]      = (tmp >> r2) | (tmp << (8 - r2));

        buf[i] ^= r1;
    }

    printf("%s\n", &buf[4]);

    return 0;
}

