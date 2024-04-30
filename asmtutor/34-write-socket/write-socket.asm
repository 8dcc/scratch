
%include "simple-lib.asm"
%include "../syscalls.asm"

section .bss
    buf     resb    255         ; For storing the data we read from the socket

section .data
    ; Our response data for sys_write
    response db 'HTTP/1.1 200 OK', 0Dh, 0Ah, 'Content-Type: text/html', 0Dh, 0Ah, 'Content-Length: 14', 0Dh, 0Ah, 0Dh, 0Ah, 'Hello World!', 0Dh, 0Ah, 0h

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
    push    byte 0              ; Same as ../32-*
    push    byte 0
    push    edi
    mov     ecx, esp

    mov     ebx, 5              ; Subroutine accept
    mov     eax, sys_socketcall
    int     0x80

.fork:
    mov     esi, eax            ; Same as ../33-* 

    mov     eax, sys_fork
    int     0x80

    cmp     eax, 0
    jz      .read

    jmp     .accept

.read:
    mov     edx, 255            ; Same as ../33-*
    mov     ecx, buf
    mov     ebx, esi
    mov     eax, sys_read
    int     0x80

    mov     eax, buf
    call    println

.write:
    mov     edx, 78
    mov     ecx, response
    mov     ebx, esi            ; Write the 78 bytes from response to the same FD we got from the accept subroutine, stored in esi,
                                ; and used for sys_read
    mov     eax, 4
    int     0x80

.exit:
    call    quit

