%include "simple-lib.asm"

section .text
    global _start

_start:
    mov     eax, 90     ; First num
    call    iprint      ; Print first num
    push    eax         ; Store old eax

    mov     eax, 45     ; For printing '-'
    call    cprint

    mov     ebx, 9      ; Second num
    mov     eax, ebx    ; Move new num to eax for iprint
    call    iprint
    
    mov     eax, 61     ; For printing '='
    call    cprint

    pop     eax         ; Pop old eax for actual operation
    sub     eax, ebx    ; eax -= ebx
    call    iprintln    ; Print result using function from ../11-*

    call    quit
