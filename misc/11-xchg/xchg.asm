
%include "simple-lib.asm"

section .data
    test_s  dd  69

section .text
    global _start

_start:
    mov     ebx, 1337

    mov     eax, 420
    call    puti                    ; 420

    xchg    eax, ebx
    call    puti                    ; 1337

    xchg    eax, dword [test_s]
    call    puti                    ; 69

    mov     eax, dword [test_s]     ; test_s changed
    call    puti                    ; 1337

    mov     eax, 0xDEADC0DE

    xchg    eax, dword [test_s]     ; test_s is 0xDEADC0DE after this xchg call
    call    puti                    ; 1337

    call    quit

