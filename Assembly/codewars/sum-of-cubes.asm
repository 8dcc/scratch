; https://www.codewars.com/kata/59a8570b570190d313000037
; RAX sumcubes(DI n)

section .text
global sumcubes

sumcubes:
    xor   rax, rax
    xor   rdx, rdx
    
    mov   rcx, 1      ; i = 1;
.loop:
    cmp   rcx, rdi    ; if (i > arg0)
    jg    .done       ;   break;
    
    ; Save cube of rcx in rax
    push  rdx         ; Save for mul
    mov   rax, rcx    ; rax  = i;
    mul   rcx         ; rax *= i; (^2)
    mul   rcx         ; rax *= i; (^3)
    pop   rdx         ; Restore from mul
    
    ; Accumulate final value in rdx
    add   rdx, rax    ; ret += rax; (rax is rcx^3)
    
    inc   rcx         ; i++;
    jmp   .loop       ; continue;

.done:
    mov   rax, rdx
    ret

