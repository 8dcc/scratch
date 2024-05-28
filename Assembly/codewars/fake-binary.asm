; Kata: https://www.codewars.com/kata/57eae65a4321032ce000002d
;
; for (; *rdi != '\0'; rdi++; rsi++)
;     if (*rdi < '5')
;         *rsi = '0';
;     else
;         *rsi = '1';

section .text

global fakebin
fakebin:
    mov    rax, rsi

.char_loop:
    cmp    byte [rdi], 0        ; if (*rdi == '\0')
    jz    .done                 ;     break
    
    cmp    byte [rdi], '5'      ; if (*rdi < '5')
    jge    .five_or_more        ;
    mov    byte [rsi], '0'      ;     *rsi = '0'
    jmp    .continue            ;     continue

.five_or_more:                  ; else
    mov    byte [rsi], '1'      ;     *rsi = '1'

.continue:
    inc    rdi                  ; rdi++
    inc    rsi                  ; rsi++
    jmp    .char_loop           ; continue

.done:
    ret
