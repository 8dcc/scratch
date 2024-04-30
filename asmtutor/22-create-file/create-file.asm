
%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0

section .text
    global _start

_start:
    mov     ecx, 0777o      ; Permissions in octal
    mov     ebx, filename   ; "test.txt", the file we want to create
    mov     eax, 8          ; sys_creat (kernel opcode 8)
    int     0x80

    call    quit

