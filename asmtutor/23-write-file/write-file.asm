
%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0
    contents    db      "Hello, world!", 0x0

section .text
    global _start

_start:
    mov     ecx, 0777o      ; Create a file, same as ../22-*
    mov     ebx, filename
    mov     eax, 8          ; sys_creat
    int     0x80            ; After calling sys_creat, the file descriptor is loaded into eax

    push    eax             ; Push the file descriptor to the stack because we want to get the string length
    mov     eax, contents
    call    slen            ; Get the string length of contents
    mov     edx, eax        ; Move the string len to edx, which will be a parameter for sys_write
    pop     eax             ; Get back file descriptor from the stack
    
    ;mov    edx, slen(contents)
    mov     ecx, contents   ; Second parameter of sys_write is the str ptr
    mov     ebx, eax        ; Move the file descriptor we just poped to ebx, the first argument of sys_write
    mov     eax, 4          ; sys_write kernel opcode
    int     0x80

    call    quit
