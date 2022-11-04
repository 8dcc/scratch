
%include "simple-lib.asm"

section .data
    prompt  db  "Enter your name: ", 0x0
    welcome db  "Hello, ", 0x0

section .bss
    input_buf   resb    255     ; Reserve 255 bytes

section .text
    global _start

_start:
    mov     eax, prompt
    call    sprint

    mov     edx, 255        ; 4th arg of sys_read is buf size, in this case the reserved bytes
    mov     ecx, input_buf  ; buf ptr
    mov     ebx, 0          ; stdin (remember that stdout is 1)
    mov     eax, 3          ; sys_read (opcode 3)
    int     0x80

    mov     eax, welcome
    call    sprint

    mov     eax, input_buf
    call    sprint

    call quit

