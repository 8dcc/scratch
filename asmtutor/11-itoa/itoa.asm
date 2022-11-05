%include "simple-lib.asm"

section .data
    global _start

_start:
    mov     eax, 420
    call    iprintln

    mov     eax, 1337
    call    iprintln

    mov     ebx, eax    ; Move eax to exit code to make sure eax remains unchanged (poped) after iprint
    mov     eax, 1
    int     0x80
