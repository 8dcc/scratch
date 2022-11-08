%include "simple-lib.asm"

section .data
    remstr  db  ", remainder: "

section .text
    global _start

_start:
    mov     eax, 93     ; First num
    call    iprint      ; Print first num
    push    eax         ; Store old eax

    mov     eax, 47     ; For printing '/'
    call    cprint

    mov     ebx, 9      ; Second num
    mov     eax, ebx    ; Move new num to eax for iprint
    call    iprint
    
    mov     eax, 61     ; For printing '='
    call    cprint

    pop     eax         ; Pop old eax for actual operation
    div     ebx         ; edx = eax % ebx; eax /= ebx
    call    iprint      ; Print result using function from ../11-*

    mov     eax, remstr ; ", remainder: "
    call    sprint
    mov     eax, edx    ; edx is the remainder
    call    iprintln

    call    quit
