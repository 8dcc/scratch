; We will first create and write some contents to a file (same as lesson 22 and 23),
; then open it (same as lesson 24), and then read the contents of the file.

%include "simple-lib.asm"

section .data
    filename    db      "test.txt", 0x0
    w_contents  db      "Hello, world!", 0x0        ; Contents we will write with sys_write

    p_msg       db      "Parent process got FD: ", 0x0
    c_msg       db      "Child process got FD: ", 0x0

section .text
    global _start

_start:
    mov     ecx, 0777o      ; Same as 22-*
    mov     ebx, filename
    mov     eax, 8          ; sys_creat
    int     0x80

    ;mov    edx, slen(contents)
    push    eax             ; Same as 23-*
    mov     eax, w_contents
    call    slen
    mov     edx, eax
    pop     eax

    mov     ecx, w_contents
    mov     ebx, eax
    mov     eax, 4          ; sys_write
    int     0x80

    mov     eax, 2          ; sys_fork
    int     0x80
    
    cmp     eax, 0          ; Compare returned value to 0 to check if we are in the parent or child process.
    jz      .child          ; See 20-*

.parent:
    lea     eax, p_msg      ; "Parent process got FD: ". I'm using "lea" here just to show that it would work the same way as
                            ; "mov". I read somewhere that when you load a label into a register using "mov", nasm is actually
                            ; using "lea",  but I am not sure so I just wanted to leave the example here.
    call    sprint
    jmp     .done

.child:
    lea     eax, c_msg      ; "Child process got FD: "
    call    sprint
    jmp     .done

.done:
    mov     ecx, 0          ; Same as 24-*
    mov     ebx, filename
    mov     eax, 5          ; sys_open
    int     0x80

    call    iprintln        ; Print file descriptor

    mov     ebx, eax        ; Move the file descriptor we just got from sys_open to ebx
    mov     eax, 6          ; sys_close
    int     0x80

    call    quit
