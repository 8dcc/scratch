; Note: This file was made after other ones because the folder order was wrong and 03 was missing,
; so check 04 for original comments

SECTION .data
    hello_world     db  "Hello, world!", 0xA

SECTION .text
    global _start

_start:
    mov     ebx, hello_world    ; Make eax and ebx point to the same string
    mov     eax, ebx

nextchar:
    cmp     byte[eax], 0        ; Check if the current byte that eax is pointing to is '\0'
    jz      finished            ; (if)       If it's 0 we are done
    inc     eax                 ; (else)     Increment the eax ptr by 1, go to next char of str
    jmp     nextchar            ; (continue) Check the new char again

finished:
    sub     eax, ebx            ; (eax -= ebx) eax will be now pointing to the end of the str,
                                ; so we subtract the difference between the 2 ptrs.

    mov     edx, eax            ; str len
    mov     ecx, hello_world    ; char* str
    mov     ebx, 1              ; stdout
    mov     eax, 4              ; write
    int     0x80                ; Call kernel

    mov     ebx, 0              ; Exit code
    mov     eax, 1              ; Exit syscall
    int     0x80                ; Call kernel

