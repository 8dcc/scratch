; Compile using '-g' for debug symbols:
;   nasm -f elf -g register-debugging.asm
;   ld -m elf_i386 -o register-debugging.out register-debugging.o

section .text
    global _start

_start:
    mov     eax, 420
    mov     ebx, eax
    sub     ebx, 5

    mov     ebx, 0
    mov     eax, 1
    int     0x80

