
section .data
    msg1        db  "Hello world!", 0xA
    msg1_len    equ $-msg1

section .text
    global _start

print_msg:
    mov     edx, msg1_len
    mov     ecx, msg1
    mov     ebx, 1                  ; STDOUT
    mov     eax, 4                  ; SYS_WRITE
    int     0x80
    ret

_start:
    call    print_msg
    mov     [msg1+5], byte '_'
    call    print_msg

    mov     ebx, 0                  ; Exit code
    mov     eax, 1                  ; SYS_EXIT
    int     0x80
