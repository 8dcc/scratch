; We will first create and write some contents to a file (same as lesson 22 and 23),
; and then open it using sys_open(O_RDONLY, "test.txt"), and printing the returned
; file descriptor.

%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0
    contents    db      "Hello, world!", 0x0

section .data
    global _start

_start:
    mov     ecx, 0777o      ; Same as ../22-*
    mov     ebx, filename
    mov     eax, 8          ; sys_creat
    int     0x80


    ;mov    edx, slen(contents)
    push    eax             ; Same as ../23-*
    mov     eax, contents
    call    slen
    mov     edx, eax
    pop     eax

    mov     ecx, contents
    mov     ebx, eax
    mov     eax, 4          ; sys_write
    int     0x80

    mov     ecx, 0          ; ecx is the access mode flag:
                            ;   0 - O_RDONLY
                            ;   1 - O_WRONLY
                            ;   2 - O_RDWR
    mov     ebx, filename   ; ebx is the filename
    mov     eax, 5          ; sys_open kernel opcode
    int     0x80

    call    iprintln        ; Print the returned integer from sys_open

    call    quit
