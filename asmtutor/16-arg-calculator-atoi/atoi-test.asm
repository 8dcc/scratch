
%include 'simple-lib.asm'

section .data
    testmsg db  "1234..."

section .text
    global _start

_start:
    mov     eax, testmsg    ; "1234..."
    call    atoi            ; 1234
    call    iprintln        ; Print number

    call    quit            ; Return 0
