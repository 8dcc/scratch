/* https://app.hackthebox.com/challenges/366
 *
 * Rebuilt version of the original executable (Except it reads/writes to
 * stdin/stdout instead of flags.enc).
 * Usage:
 *   ./encrypt.out < input.txt > output.txt
 */

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
    *ptr = '\0';

    /* main+0xA6 -> main+0xB8 */
    uint32_t seed = time(NULL);
    srand(seed);

    fprintf(stderr, "Seed: %d\n", seed);

    for (int i = 0; buf[i] != '\0'; i++) {
        /* main+0xC7 -> main+0xCC */
        uint32_t r1 = rand();

        /* main+0xCF -> main+0xF2 */
        buf[i] ^= r1;

        /* main+0xF4 -> main+0xFC */
        uint32_t r2 = rand() & 7;

        /* main+0xFE -> main+0x10C */
        uint8_t tmp = buf[i];

        /* main+0x10F -> main+0x130 (optimized) */
        buf[i] = (tmp << r2) | (tmp >> (8 - r2));
    }

    /* main+0x15C -> main+0x170 */
    fwrite(&seed, sizeof(char), 4, stdout);
    printf("%s", &buf[0]);

    return 0;
}
