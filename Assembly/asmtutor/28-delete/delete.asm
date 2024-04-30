
%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0

section .text
    global _start

_start:
    mov     ebx, filename       ; The only parameter of sys_unlink is the filename
    mov     eax, 10             ; sys_unlink (kernel opcode 10)
    int     0x80

    call    quit
