
%include "simple-lib.asm"

section .data
    child_msg       db      "I am the child process!", 0x0
    parent_msg      db      "I am the parent process!", 0x0

section .text
    global _start

_start:
    mov     eax, 2      ; sys_fork (kernel opcode 2)
    int     0x80
    
    cmp     eax, 0     ; From here the 2 processes will continue, we can check what process we are in by checking eax,
                       ; so one process executes something and the child, another part of the code.
                       ; eax == 0 means we are in the child process
    jz      child

parent:                ; else
    mov     eax, parent_msg
    call    println

    call    quit

child:
    mov     eax, child_msg
    call    println

    call    quit

