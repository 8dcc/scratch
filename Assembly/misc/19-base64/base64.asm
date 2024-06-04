; NOTE: For a C version, see '../../../C/crypto/base64.c'.

%define PADDING_CHAR 0x3D ; '='

default rel

section .data
base64_chars:
    db "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

section .text

; char* base64(char* dst, const void* src, size_t sz);
global base64
base64:
    push    rbp
    mov     rbp, rsp

    ; Preserve the original `dst' pointer.
    push    rdi

    ; Use `rax' for the base64 character array.
    lea     rax, [base64_chars]

.loop:
    ; On every iteration, we will write 4 characters and read 3 bytes.
    ; Are we done with the input?
    cmp     rdx, 0
    jle     .done

    ; Higher 6 bits of the 1st byte. Uses `r9' (which contains those 6 bits) as
    ; index for the base64 character array (in `rax').
    movzx   r9, byte [rsi]
    shr     r9, 2

    ; Write the 1st character of the output, using the resulting 6 bits as index
    ; for the character array.
    mov     r8b, byte [rax + r9]
    mov     byte [rdi], r8b
    inc     rdi

    ; Before moving to the 2nd input byte, save the 1st one.
    movzx   r9, byte [rsi]

    ; Move to the 2nd byte of the input.
    inc     rsi
    dec     rdx

    ;---------------------------------------------------------------------------
    ; Check if there are 2 characters left in the input.
    test    rdx, rdx
    jnz     .two_chars_left

    ; No characters left, add padding and return.
    mov     byte [rdi], PADDING_CHAR
    inc     rdi
    mov     byte [rdi], PADDING_CHAR
    inc     rdi
    jmp     .done

.two_chars_left:
    ; OR the lower 2 bits of the 1st input byte (shifted to bits 4..5) with the
    ; higher 4 bits of the 2nd input byte (shifted to bits 0..3).
    and     r9, 0x3
    shl     r9, 4
    movzx   r8, byte [rsi]
    shr     r8, 4
    or      r9, r8

    ; Write the 2nd character of the output.
    mov     r8b, byte [rax + r9]
    mov     byte [rdi], r8b
    inc     rdi

    ; Before moving to the 3rd input byte, save the 2nd one.
    movzx   r9, byte [rsi]

    ; Move to the 3rd byte of the input.
    inc     rsi
    dec     rdx

    ;---------------------------------------------------------------------------
    ; Check if there is a character left in the input.
    test    rdx, rdx
    jnz     .one_char_left

    ; No characters left, add padding and return.
    mov     byte [rdi], PADDING_CHAR
    inc     rdi
    jmp     .done

.one_char_left:
    ; OR the lower 4 bits of the 2nd input byte (shifted to bits 2..5) with the
    ; higher 2 bits of the 2nd input byte (shifted to bits 0..1).
    and     r9, 0xF
    shl     r9, 2
    movzx   r8, byte [rsi]
    shr     r8, 6
    or      r9, r8

    ; Write the 3rd character of the output.
    mov     r8b, byte [rax + r9]
    mov     byte [rdi], r8b
    inc     rdi

    ;---------------------------------------------------------------------------
    ; Write the last char of the output, corresponding to the lower 6 bits of
    ; the 3rd input byte.
    movzx   r9, byte [rsi]
    and     r9, 0x3F

    ; Write the 4th character of the output.
    mov     r8b, byte [rax + r9]
    mov     byte [rdi], r8b
    inc     rdi

    ; Move to the next input character, and loop again.
    inc     rsi
    dec     rdx
    jmp     .loop

.done:
    ; Null-terminate the output string
    mov     byte [rdi], 0

    ; Return the original `dst' pointer (rdi)
    pop     rax

    mov     rsp, rbp
    pop     rbp
    ret
