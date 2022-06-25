%include 'my-include.asm'

section .data
    str1    db      "I am a test string!", 0x0
    str2    db      "I am also a different test string!", 0x0

section .text
    global _start

_start:
    mov     eax,str1        ; Move str1 to eax (as func argument)
    call    sprintlf        ; Call sprint function from my-include.asm

    mov     eax,str2
    call    sprintlf        ; Print str2

    call    quit            ; Call quit function from my-include.asm
