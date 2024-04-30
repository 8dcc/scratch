; Compile with '-g' and inspect with gdb using:
;   gdb> x/10b $esp
; (Show 10 bytes of the address contained in the esp register)

%include "simple-lib.asm"

section .text
    global _start

_start:
    push    byte 69
    push    byte 0x6
    push    byte 0x9
    pop     eax
    pop     eax
    pop     eax

    push    word 0x1337
    push    dword 0x1337
    pop     eax
    pop     eax

    call    quit
