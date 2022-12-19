
%include "simple-lib.asm"
%include "../syscalls.asm"

section .bss
    buf     resb    255         ; For storing the data we read from the socket

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
    push    byte 1
    push    edi
    mov     ecx, esp

    mov     ebx, 4              ; Subroutine listen
    mov     eax, sys_socketcall
    int     0x80

.accept:
    push    byte 0              ; Same ass ../32-*
    push    byte 0
    push    edi
    mov     ecx, esp

    mov     ebx, 5              ; Subroutine accept
    mov     eax, sys_socketcall
    int     0x80

.fork:
    mov     esi, eax            ; The accept subroutine will return a FD, that we will use to read data from the socket. We save that FD in esi

    mov     eax, sys_fork       ; Fork our program
    int     0x80

    cmp     eax, 0              ; If we are the child, jump to .read
    jz      .read

    jmp     .accept             ; If we are the parent, jump back to .accept
                                ; TODO: What stops the parent loop from creating infinite accept subroutines?

.read:
    mov     edx, 255            ; Number of bytes to read. This is just a normal sys_read call but from the FD we got from the accept subroutine
    mov     ecx, buf            ; Where to save the data from sys_read
    mov     ebx, esi            ; Move to ebx the FD we got from the accept subroutine, and saved in esi in .fork
    mov     eax, sys_read
    int     0x80

    mov     eax, buf            ; Print what we just read
    call    println

.exit:
    call    quit
