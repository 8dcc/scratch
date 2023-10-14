; Used to test performance of loops. After compiling, run with:
;   $ time ./better-for-loop.out
; Comment the following line and check the difference. For me:
;   BETTER_LOOP defined:
;     real    1m3.449s
;     user    1m3.259s
;     sys     0m0.076s
;   BETTER_LOOP commented:
;     real    1m10.191s
;     user    1m9.955s
;     sys     0m0.113s
;
; This program was inspired by Page 57 of "Reversing: Secrets of Reverse
; Engineering".

%define BETTER_LOOP

%define ITERS    999999
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
; void for_test(int num)
;     ecx = 0
;     if (ecx >= ITERS)
;         return;
;
;     do {
;         if (isprime(ecx))
;             printf("Prime: %d\n", edx);
;         ecx++;
;     } while (ecx < ITERS);
for_test:
    push    ebx
    push    ebp
    mov     ebp, esp

    mov     ebx, [ebp + 12]     ; ebx = arg0;
    mov     ecx, 1

    cmp     ecx, ebx            ; if (ecx >= arg0)
    jge     .done               ;     return;

.loop:
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
    cmp     ecx, ebx            ; if (ecx < arg0)
    jl      .loop               ;     continue;

.done:
    mov     esp, ebp
    pop     ebp
    pop     ebx
    ret
%else
; void for_test(int num)
;     ecx = 0;
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

; NOTE: These are not best the methods because it's used to check performance of
; the for_test function above.
%ifdef BETTER_LOOP
; int isprime(int eax)
;     ecx = 2;
;     if (ecx >= eax)
;         return 1;
;
;     do {
;         if (ecx % eax == 0)
;             return 0;
;         ecx++;
;     } while (ecx < eax);
;     return 1;
isprime:
    push    ecx             ; Save used registers
    push    edx

    mov     ecx, 2

    cmp     ecx, eax        ; if (ecx >= eax)
    jge     .prime          ;     return true;

.loop:
    push    eax             ; Number we want to check
    xor     edx, edx        ; Clear edx for division remainder
    div     ecx             ; edx = eax % ecx; eax /= ecx;
    pop     eax             ; Restore original number for next iteration

    test    edx, edx        ; if (eax % ecx == 0)
    jz      .not_prime      ;     return false;

    inc     ecx             ; ecx++;
    cmp     ecx, eax        ; if (ecx < eax)
    jl      .loop           ;     continue;

.prime:
    mov     eax, 1          ; else
    jmp     .done           ;     return true;

.not_prime:
    mov     eax, 0

.done:
    pop     edx             ; Restore used registers
    pop     ecx
    ret
%else
; int isprime(int eax)
;     ecx = 2
;     while (ecx <= eax)
;         if (ecx % eax == 0)
;             return 0;
;         ecx++
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
%endif
