; Kata: https://www.codewars.com/kata/542c0f198e077084c0000c2e

section .text

; int divisors(int edi) 
global divisors
divisors:
    push    rbx                     ; Preserve registers
    push    rcx
    push    rdx

    xor     rbx, rbx                ; ret = 0;
    mov     rcx, rdi
.divisor_loop:
    test    rcx, rcx                ; for (ecx = arg0; ecx != 0; ecx--)
    jz      .done

    xor     rdx, rdx                ; Used by div
    mov     rax, rdi
    div     rcx                     ; edx = eax % ecx; eax /= ecx;

    test    rdx, rdx                ; Not zero, has remainder
    jnz     .has_remainder
    inc     rbx                     ; If no remainder, divisible, ebx++;

.has_remainder:
    dec     rcx
    jmp     .divisor_loop

.done:
    mov     rax, rbx                ; Return ebx

    pop     rdx                     ; Restore registers
    pop     rcx
    pop     rbx
    ret
