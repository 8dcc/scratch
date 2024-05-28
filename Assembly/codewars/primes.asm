; Kata: https://www.codewars.com/kata/5262119038c0985a5b00029f
; bool is_prime(int num)

section .text
global is_prime

is_prime:
    cmp     rdi, 2          ; if (arg0 < 2)
    jl      .not_prime      ;   return false;

    mov     rax, rdi        ; rax = arg0
    xor     rdx, rdx        ; For the division
    mov     rbx, 2
    div     rbx             ; eax /= 2
    mov     rbx, rax        ; Save half of original arg0 into ebx (for the loop)

    ; for (rcx = 2; rcx <= rax / 2; rcx++)
    ;   if (rcx % rax == 0)
    ;     return 0;
    mov     ecx, 2

.loop:
    ; Check if rcx is greater than half of the original rax. n/2 is the biggest
    ; possible divisor of n. We are done with the loop and there were no
    ; divisors, we can return that the number is prime.
    cmp     rcx, rbx        ; if (rcx > rax/2)
    jg      .prime          ;   return true;

    mov     rax, rdi        ; Move original number to rax for division
    xor     rdx, rdx        ; Clear edx for division remainder
    div     rcx             ; rdx = rax % ecx;
                            ; rax /= rcx;

    ; rax is divisible by the current iteration, not prime
    test    rdx, rdx        ; if (arg0 % i == 0)
    jz      .not_prime      ;   return false;

    inc     ecx             ; i++;
    jmp     .loop           ; continue;

.prime:
    mov     eax, 1          ; return true;
    jmp     .done

.not_prime:
    mov     eax, 0          ; return false;
    ;jmp     .done

.done:
    ret

