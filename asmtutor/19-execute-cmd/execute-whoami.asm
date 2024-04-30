
; For comments see execute-cmd.asm

%include "simple-lib.asm"

section .data
    cmd     db      "/usr/bin/whoami", 0x0

    args    dd      cmd     ; We don't want to pass aditional args but we still need to have the cmd itself
            dd      0x0

    env     dd      0x0

section .text
    global _start

_start:
    mov     edx, env
    mov     ecx, args
    mov     ebx, cmd
    mov     eax, 11
    int     0x80

    call    quit
