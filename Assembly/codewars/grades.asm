; Kata: https://www.codewars.com/kata/5ad0d8356165e63c140014d4
; unsigned final_grade(unsigned exam, unsigned nproj)

section .text
global final_grade

final_grade:
    xor   rax, rax
    
    cmp   rdi, 90         ; if (arg0 > 90 || arg1 >= 10)
    jg    .ret_100        ;   return 100
    cmp   rsi, 10
    jg    .ret_100
    
    cmp   rdi, 75         ; else if (arg0 <= 75)
    jle   .check_50
    cmp   rsi, 5          ;   if (arg1 >= 5)
    jge   .ret_90         ;     return 90
    
.check_50:
    cmp   rdi, 50         ; else if (arg0 <= 50)
    jle   .done
    cmp   rsi, 2          ;   if (arg1 >= 2)
    jge   .ret_75         ;     return 75
    jmp   .done           ; else return 0

; ------------------------------------------------

.ret_100:
    mov   rax, 100
    jmp   .done

.ret_90:
    mov   rax, 90
    jmp   .done

.ret_75:
    mov   rax, 75
    ;jmp   .done

.done:
    ret
    
