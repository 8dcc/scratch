; We will first create and write some contents to a file (same as lesson 22 and 23),
; then open it (same as lesson 24), and then read the contents of the file.

%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0
    w_contents  db      "Hello, world!", 0x0        ; Contents we will write with sys_write

section .bss
    r_contents  resb    255                         ; Reserve space for when we read using sys_read

section .text
    global _start

_start:
    mov     ecx, 0777o      ; Same as ../22-*
    mov     ebx, filename
    mov     eax, 8          ; sys_creat
    int     0x80


    ;mov    edx, slen(contents)
    push    eax             ; Same as ../23-*
    mov     eax, w_contents
    call    slen
    mov     edx, eax
    pop     eax

    mov     ecx, w_contents
    mov     ebx, eax
    mov     eax, 4          ; sys_write
    int     0x80

    mov     ecx, 0          ; Same as ../24-*
    mov     ebx, filename
    mov     eax, 5          ; sys_open
    int     0x80

    mov     edx, 13         ; Number of bytes we want to read
    mov     ecx, r_contents ; Reserved space for reading
    mov     ebx, eax        ; File descriptor we got from sys_open
    mov     eax, 3          ; sys_read (kernel opcode 3)
    int     0x80

    mov     eax, r_contents
    call    println

    call    quit
