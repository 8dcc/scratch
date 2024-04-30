; We will first create a file (lesson 22), write some contents (lesson 23), open it (lesson 24),
; and then write some contents at an offset.

%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0
    w_contents  db      "123456789", 0xA, "123456789", 0x0      ; Contents we will write with sys_write
    s_contents  db      "test!", 0x0                            ; Contents we will write with sys_lseek

section .text
    global _start

_start:
    mov     ecx, 0777o      ; Same as ../22-*
    mov     ebx, filename
    mov     eax, 8          ; sys_creat
    int     0x80

    mov     ebx, eax        ; fwrite(char* s, unsigned int fd)
    mov     eax, w_contents
    call    fwrite          ; New change to simple-lib.asm

    mov     ecx, 1          ; Open in O_WRONLY mode. See ../24-*
    mov     ebx, filename
    mov     eax, 5          ; sys_open
    int     0x80

    ; --------------------------------------------------------------

    mov     edx, 2          ; Whence argument for sys_lseek. Can be:
                            ;   0 - SEEK_SET - Begining of the file
                            ;   1 - SEEK_CUR - Current file offset
                            ;   2 - SEEK_END - End of the file
    mov     ecx, 0          ; Offset. In this case 0 because the end of the file is where we want to write.
    mov     ebx, eax        ; File descriptor we got from sys_open
    mov     eax, 19         ; sys_lseek (kernel opcode 19)
    int     0x80

    mov     eax, s_contents ; "test!"
    mov     ebx, ebx        ; File descriptor was already in ebx. We don't need to change the FD because
                            ; afaik sys_lseek will change the position we will write to with the same FD.
    call    fwrite

    mov     edx, 0          ; Begining of the file this time
    mov     ecx, 3          ; With 5 bytes of offset
    mov     ebx, ebx        ; File descriptor was already in ebx.
    mov     eax, 19         ; sys_lseek
    int     0x80

    mov     eax, s_contents ; "test!"
    mov     ebx, ebx
    call    fwrite

    ; --------------------------------------------------------------

    mov     ebx, ebx
    mov     eax, 6          ; sys_close
    int     0x80

    call    quit
