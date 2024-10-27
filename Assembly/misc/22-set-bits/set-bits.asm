
bits 64
default rel

section .bss
    user_input:  resd 1

section .data
    str_prompt:  db "Number: ", 0x0
    str_scanfmt: db "%d", 0x0
    str_outfmt:  db "Done: %#lx", 0xA, 0x0

section .text
    extern printf:function
    extern scanf:function
    extern exit:function

global main
main:
    push    rbp                 ; NOTE: The stack has to be aligned to 16 bytes
    mov     rbp, rsp            ; before calling functions, according to the ABI

.loop:
    lea     rdi, [str_prompt]
    mov     rax, 0              ; Number of va_args, needed by `printf'
    call    printf wrt ..plt    ; printf("Number: ")

    lea     rsi, [user_input]
    lea     rdi, [str_scanfmt]
    mov     rax, 1
    call    scanf wrt ..plt     ; scanf("%d", &user_input)

    cmp     rax, 1
    jl      .loop               ; Call to `scanf' failed, ask for data again.

    mov     edi, dword [user_input]
    call    setbits             ; Set the specified number of bits.

    mov     rsi, rax
    lea     rdi, [str_outfmt]
    mov     rax, 1
    call    printf wrt ..plt    ; printf("Done: %#lx\n", result)

    mov     rdi, 0
    call    exit wrt ..plt

; Set the least significant N bits of a number. Inspired by the Hacker's Delight
; book, by Henry S. Warren, Jr.
;
;    uint64_t setbits(uint8_t n) {
;        return (1 << (n + 1)) - 1;
;    }
global setbits
setbits:
    mov     cl, dil
    inc     cl
    mov     rax, 1
    shl     rax, cl
    dec     rax
    ret
