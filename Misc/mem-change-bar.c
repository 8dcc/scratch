/* Test intended for fs-os/fs-os */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define SCREEN_H 5
#define SCREEN_W 20

#define COL_SAME false
#define COL_DIFF true

uint8_t screen[SCREEN_H * SCREEN_W];

#define BASE_HASH    5381
#define MAGIC_NUMBER 33

uint32_t hash_region(void* start, void* end) {
    uint32_t hashed = BASE_HASH;

    for (uint8_t* ptr = start; (uint64_t)ptr < (uint64_t)end; ptr++)
        hashed = (hashed * MAGIC_NUMBER) ^ *ptr;

    return hashed;
}

static uint32_t* old_hashes = NULL;
void ptr_changes(uint32_t px_s, uint32_t px_e, void* mem_s, void* mem_e) {
    static const uint32_t y = 2;

    const uint32_t bar_sz = px_e - px_s;
    const uint32_t mem_sz = mem_e - mem_s;

    /* 1 entry per px */
    if (!old_hashes)
        old_hashes = calloc(bar_sz + 1, sizeof(uint32_t));

    /* Bytes that will be represented in pixel of the bar */
    const uint32_t reg_sz = mem_sz / bar_sz;
    for (uint32_t x = 0; x <= bar_sz; x++) {
        void* reg_start = mem_s + (x * reg_sz);
        void* reg_end   = reg_start + reg_sz;

        /* In case mem_e is weird not even number and we went past it */
        if (reg_end > mem_e + 1)
            reg_end = mem_e + 1;

        uint32_t hash = hash_region(reg_start, reg_end);

        screen[y * SCREEN_W + (px_s + x)] = (old_hashes[x] == hash) ? COL_SAME
                                                                    : COL_DIFF;

        /* Store hash for next call */
        old_hashes[x] = hash;
    }
}

/* For testing */
void print_screen() {
    puts("Screen:");

    for (int y = 0; y < SCREEN_H; y++) {
        for (int x = 0; x < SCREEN_W; x++) {
            if (screen[y * SCREEN_W + x] == true)
                putchar('#');
            else if (screen[y * SCREEN_W + x] == false)
                putchar('-');
            else
                putchar('.');
        }

        putchar('\n');
    }
}

int main() {
    /* For testing, supposed to be screen and memory regions */
    uint8_t mem[500] = { 0 };

    for (int y = 0; y < SCREEN_H; y++)
        for (int x = 0; x < SCREEN_W; x++)
            screen[y * SCREEN_W + x] = 2;

    ptr_changes(2, 16, &mem[100], &mem[400]);
    print_screen();

    mem[101] = 0x12;
    mem[125] = 0x03;
    mem[320] = 0x03;
    mem[400] = 0x03;

    ptr_changes(2, 16, &mem[100], &mem[400]);
    print_screen();

    mem[50] = 0x03;

    /* No changes inside region (100..400), should be all '.' */
    ptr_changes(2, 16, &mem[100], &mem[400]);
    print_screen();

    /* Not sure how we would free before killing the bar task in fs-os */
    free(old_hashes);
    return 0;
}
