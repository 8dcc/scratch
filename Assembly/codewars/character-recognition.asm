; Kata: https://www.codewars.com/kata/577bd026df78c19bca0002c0
;
; for (; *rdi != '\0'; rdi++)
;     if (*rdi == '5')
;         *rdi = 'S';
;     else if (*rdi == '0')
;         *rdi = 'O';
;     else if (*rdi == '1')
;         *rdi = 'I';

section .text

global correct
correct:
    mov    rax, rdi

.char_loop:
    cmp    byte [rdi], 0        ; if (*rdi == '\0')
    jz    .done                 ;     break
    
    cmp    byte [rdi], '5'      ; if (*rdi == '5')
    jne    .not_s               ;
    mov    byte [rdi], 'S'      ;     *rdi = 'S'
    jmp    .continue            ;     continue

.not_s:
    cmp    byte [rdi], '0'      ; ...
    jne    .not_o
    mov    byte [rdi], 'O'
    jmp    .continue

.not_o:
    cmp    byte [rdi], '1'      ; ...
    jne    .continue
    mov    byte [rdi], 'I'

.continue:
    inc    rdi                  ; rdi++
    jmp    .char_loop           ; continue

.done:
    ret

