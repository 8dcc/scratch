; See also:
;   - https://stackoverflow.com/questions/22388745/how-does-conditional-jump-instruction-work-with-the-sub-operand
;   - https://reverseengineering.stackexchange.com/questions/4656/is-or-eax-eax-as-if-statement-possible
;   - https://reverseengineering.stackexchange.com/questions/4436/does-a-je-must-follow-directly-to-an-cmp
;   - https://8dcc.github.io/reversing/challenge5.html


%define SYS_EXIT 1

section .data
    fmt_is_zero  db "It's zero.", 0xA, 0x0
    fmt_not_zero db "It's not zero.", 0xA, 0x0

section .text
    extern printf:function

global main
main:
    mov     eax, 0x123
    sub     eax, 0x123
    call    is_zero             ; eax = 0 -> true

    ; Clear
    test    eax, 0x555
    call    is_zero             ; eax != 0x555 -> false

    mov     eax, 0x123
    sub     eax, 0x124
    add     eax, 1
    call    is_zero             ; eax = 0 -> true

    mov     eax, 0x123
    sub     eax, 0x125
    add     eax, 1
    call    is_zero             ; eax = 1 -> false

    mov     eax, SYS_EXIT
    mov     ebx, 0              ; exit(0);
    int     0x80

    ret

global is_zero
is_zero:
    jnz     .not_zero
    push    fmt_is_zero
    jmp     .print
.not_zero:
    push    fmt_not_zero
.print:
    call    printf
    add     esp, 4
    ret
