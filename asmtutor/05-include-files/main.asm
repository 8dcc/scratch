; Note: This file was made after other ones because the folder order was wrong and 05 was missing,
; so check the diff against 06

%include 'my-include.asm'

section .data
    str1    db      "I am a test string!", 0xA
    str2    db      "I am also a different test string!", 0xA

section .text
    global _start

_start:
    mov     eax,str1        ; Move str1 to eax (as func argument)
    call    sprint          ; Call sprint function from my-include.asm

    mov     eax,str2
    call    sprint          ; Print str2

    call    quit            ; Call quit function from my-include.asm
