
%include "simple-lib.asm"
%include "../syscalls.asm"

section .text
    global _start

_start:
    xor     eax, eax            ; Set registers to 0
    xor     ebx, ebx
    xor     ecx, ecx
    xor     edx, edx

.socket:
    push    byte 6              ; Same as ../29-*
    push    byte 1
    push    byte 2
    mov     ecx, esp

    mov     ebx, 1              ; Subroutine socket
    mov     eax, sys_socketcall
    int     0x80

.bind:
    mov     edi, eax            ; Same as ../30-*
    push    dword 0x0           ; Padding (4 bytes)
    push    dword 0x0           ; Padding (4 bytes)
    push    dword 0x0
    push    word 0x2923
    push    word 2
    mov     ecx, esp

    push    byte 16 
    push    ecx
    push    edi
    mov     ecx, esp

    mov     ebx, 2              ; Subroutine bind
    mov     eax, sys_socketcall
    int     0x80

.listen:
    push    byte 1              ; Max queue length.
    push    edi                 ; FD we saved from the socket subroutine
    mov     ecx, esp            ; Move argument pointer for listen subroutine to ecx

    mov     ebx, 4              ; Subroutine listen
    mov     eax, sys_socketcall
    int     0x80

.exit:
    call    quit
