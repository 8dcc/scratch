
%include "simple-lib.asm"

section .text
    global _start

_start:
    xor     eax, eax        ; Set registers to 0
    xor     ebx, ebx
    xor     ecx, ecx
    xor     edx, edx

.socket:
    push    byte 6          ; IPPROTO_TCP
    push    byte 1          ; SOCK_STREAM
    push    byte 2          ; PF_INET
    mov     ecx, esp        ; Move the pointer to those 3 arguments to ecx. sys_socketcall expects the subroutine
                            ; in ebx and a pointer to an array of arguments in ecx, which is what we just created
    mov     ebx, 1          ; Subroutine SOCKET (1)
    mov     eax, 102        ; sys_socketcall (kernel opcode 102)
    int     0x80

    call    iprintln        ; Will print the file descriptor (which is returned from sys_socketcall). We get a FD
                            ; because everything is a file in linux.

.exit:
    call    quit

