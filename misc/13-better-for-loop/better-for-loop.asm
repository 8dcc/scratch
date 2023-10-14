; Used to test performance of loops. After compiling, run with:
;   $ time ./better-for-loop.out
; Comment the following line and check the difference.
;
; This program was inspired by Page 57 of "Reversing: Secrets of Reverse
; Engineering".

;%define BETTER_LOOP

%define ITERS    99999
%define SYS_EXIT 1

section .data
    fmt db "Prime: %d", 0xA, 0x0

section .text
    extern printf:function

global main
main:
    push    ITERS
    call    for_test
    add     esp, 4

    mov     eax, SYS_EXIT
    mov     ebx, 0              ; exit(0);
    int     0x80

    ret

;-------------------------------------------------------------------------------

%ifdef BETTER_LOOP
; TODO
%else
; void for_test(int num)
;     ecx = 0
;     while (ecx < ITERS)
;         if (isprime(ecx))
;             printf("Prime: %d\n", edx)
;         ecx++;
for_test:
    push    ebx
    push    ebp
    mov     ebp, esp

    mov     ebx, [ebp + 12]     ; ebx = arg0;
    mov     ecx, 1

.loop:
    cmp     ecx, ebx            ; if (ecx >= arg0)
    jge     .done               ;     break;

    mov     eax, ecx
    call    isprime

    test    eax, eax            ; if (isprime(ecx))
    jz     .continue            ;     continue;

    push    ecx
    push    fmt
    call    printf              ; printf("Prime: %d\n", edx);
    add     esp, 4
    pop     ecx

.continue:
    inc     ecx
    jmp     .loop

.done:
    mov     esp, ebp
    pop     ebp
    pop     ebx
    ret
%endif

;-------------------------------------------------------------------------------

; NOTE: Not best the method because it's used to check performance of the
; for_test function above.
;
; int isprime(int eax)
;     for (ecx = 2; ecx <= eax - 1; ecx++)
;         if (ecx % eax == 0)
;             return 0;
;     return 1;
isprime:
    push    ecx             ; Save used registers
    push    edx

    mov     ecx, 2

.loop:
    cmp     ecx, eax        ; We reached the end and there were no divisors
    jge     .prime          ; Return 1

    push    eax             ; Number we want to check
    xor     edx, edx        ; Clear edx for division remainder
    div     ecx             ; edx = eax % ecx; eax /= ecx;
    pop     eax             ; Restore original number for next iteration

    test    edx, edx        ; If the remainder is 0, eax is divisible by the
    jz      .not_prime      ; current iteration. Return 0.

    inc     ecx             ; (i++) Increase divisor check
    jmp     .loop           ; continue;

.prime:
    mov     eax, 1
    jmp     .done

.not_prime:
    mov     eax, 0

.done:
    pop     edx             ; Restore used registers
    pop     ecx
    ret
