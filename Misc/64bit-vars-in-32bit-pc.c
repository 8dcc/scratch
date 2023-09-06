/*
 * Compile with -m32, and inspect with:
 *   $ rizin 64bit-vars-in-32bit-pc.out
 *   [0x00001060]> aaa
 *   [0x00001060]> pdf @ main
 *   ...
 */

#include <stdint.h>
#include <stdio.h>

uint64_t g = 0x5555666677778888;

uint64_t sum(uint64_t a, uint64_t b) {
    (void)b;
    return a + 0xFF00FF00DEADBEEF;
}

void sump(uint64_t* p) {
    p += 0xFF00FF00DEADBEEF;
}

int main() {
    /*
     * main+0x1d    mov   dword [ebp - var_a_lo], 0x00001337
     * main+0x24    mov   dword [ebp - var_a_hi], 0x00006969
     * main+0x2b    mov   dword [ebp - var_b_lo], 0x23456789
     * main+0x32    mov   dword [ebp - var_b_hi], 0x00000001
     *              ; Arguments for sum()
     * main+0x39    push  dword [ebp - var_b_hi]    ; uint32_t arg_1
     * main+0x3c    push  dword [ebp - var_b_lo]    ; uint32_t arg_1
     * main+0x3f    push  dword [ebp - var_a_hi]    ; uint32_t arg_0
     * main+0x42    push  dword [ebp - var_a_lo]    ; uint32_t arg_0
     * main+0x45    call  sym.sum                   ; sym.sum
     * main+0x4a    add   esp, 0x10                 ; 4 dwords we just pushed
     */
    uint64_t a = 0x0000696900001337;
    uint64_t b = 0x0000000123456789;
    uint64_t c = sum(a, b);
    (void)c;

    /*
     * main+0x53    mov   dword [ebx + 0x1c], 0x33334444   ; First half, low
     * main+0x5d    mov   dword [ebx + 0x20], 0x11112222   ; Second half, high
     * main+0x67    mov   dword [ebx + 0x1c], 5            ; Set low bits again
     * main+0x71    mov   dword [ebx + 0x20], 0            ; Set high bits again
     * main+0x7b    sub   esp, 4
     * main+0x7e    lea   eax, [ebx + 0x1c]       ; Address of first half, low
     * main+0x84    push  eax                     ; uint32_t* arg0
     * main+0x85    call  sym.sump                ; sym.sump
     */
    g = 0x1111222233334444;
    g = 0x5;
    sump(&g);

    printf("%lld, %lld\n", a, g);
    return 0;
}
