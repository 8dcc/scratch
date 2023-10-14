; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
; Le irc people told me to do it and I listen blindly to everything strangers say online.

%include "simple-lib.asm"

%define MAX_N   1000
%define VALID   byte 0      ; Can be prime
%define INVALID byte 1      ; Can't be prime

section .data
    prime_str   db      " is prime.", 0x0

section .bss
    primes      resb    MAX_N       ; Byte N will be 1 if it can't be prime

section .text
    global _start


; for (i = 2; i < MAX_N; i++)
;   if (primes[i] is valid)
;     print(i);
;
;     for (j = i; j < MAX_N; j += i)
;       primes[j] = invalid;
_start:
    mov     ecx, 2                  ; (i) First prime number

.outer_loop:
    cmp     ecx, MAX_N              ; if (ecx >= MAX_N)
    jge     .outer_done             ; break;

    cmp     [primes+ecx], VALID     ; if (primes[ecx] != 0)
    jne     .inner_done             ; i++; continue;

    mov     eax, ecx
    call    printi                  ; printi(i);
    mov     eax, prime_str
    call    puts                    ; puts(" is prime.");

    mov     ebx, ecx                ; j = i;

.inner_loop:
    cmp     ebx, MAX_N              ; if (ebx >= MAX_N)
    jge     .inner_done             ; break;

    mov     [primes+ebx], INVALID   ; primes[j] = 1;

    add     ebx, ecx                ; ebx += ecx;
    jmp     .inner_loop             ; continue;

.inner_done:
    inc     ecx                     ; i++;
    jmp     .outer_loop             ; continue;

.outer_done:
    call    quit
