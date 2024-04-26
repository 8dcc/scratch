
; Same output with and without this line
default REL

section .text
    extern myGlobalInteger
    extern foo

global foo_proxy
foo_proxy:
    mov     [myGlobalInteger], rdi
    jmp     foo
