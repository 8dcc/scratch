
%include "simple-lib.asm"

section .data
    msg     db      "Unix time: ", 0x0

section .text
    global _start

_start:
    mov     eax, msg
    call    sprint

    mov     eax, 13     ; sys_time
    int     0x80

    call    iprintln
    call    quit
