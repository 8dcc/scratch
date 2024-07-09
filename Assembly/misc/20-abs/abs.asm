; ==============================================================================
; Credits for this method:
;
;   Agner Fog. Optimizing subroutines in assembly language. (Updated 2023-06-22)
;   Section 6.1: MASM style inline assembly.
;
; The basic idea for calculating the absolute value of N is the following:
;
;   1. If the value is negative, invert all bits and subtract 1.
;   2. If the value is positive, return it unchanged.
;
; To understand why this method works for negative numbers, you have to keep in
; mind how negative numbers are represented in binary. The decimal number 1 in
; binary has the lowest bit set, while the decimal number -1 has ALL bits
; set. As you can see, we can't convert between positive and negative just by
; inverting all the bits.

;   | Decimal |   Binary |
;   |---------+----------|
;   |       1 | 00000001 |
;   |       2 | 00000010 |
;   |      -1 | 11111111 |
;   |      -2 | 11111110 |
;   |   ~(-2) | 00000001 |
;
; However, there is still a clear relationship between the inverted negative and
; the positive values. After inverting a negative value N, the binary
; representation matches the number prior to the positive version of N.
;
; In other words, since there is no negative zero, negative numbers "start
; counting earlier" than the positives. This is why the last digit of INT_MIN
; always bigger than INT_MAX.
; ==============================================================================

; NOTE: You can assemble in 64-bits by modifying the Makefile, but remember
; that, acording to the ABI, the parameter will be in `rdi', not in the stack.
bits 32

section .text

; int abs(int n);
global my_abs
my_abs:
    ; Move first and only argument into `eax'.
    mov     eax, [esp + 4]

    ; Convert double-word in `eax' to quad-word in `edx:eax'. The `edx' register
    ; will contain the sign extension of `eax'. In other words, all bits of
    ; `edx' are set to bit 31 of `eax'.
    cdq

    ; If `eax' was negative, the highest bit was set, so all bits in `edx' are
    ; now set. By performing a bit-wise XOR of the two registers, we are
    ; effectively inverting all bits of `eax'.
    ;
    ; However, if `eax' was positive, the highest bit was not set, so `edx' is
    ; zero. Performing a bit-wise XOR by zero leaves the value unchanged.
    xor     eax, edx

    ; As explained, if the value was negative, the value in `eax' was inverted
    ; and all bits of `edx' are set. Remember that 0xFFFFFFFF is the binary
    ; representation of -1. If we subtract -1 from `eax', we are effectively
    ; adding 1, which is what we want to do when the value is negative.
    ;
    ; However, if the value was positive, `eax' was not inverted, and `edx' is
    ; still zero. In this case, subtracting zero from `eax' will leave the value
    ; unchanged, which is also what we want.
    sub     eax, edx

    ; We return `eax', which contains the absolute value of the input number.
    ret
