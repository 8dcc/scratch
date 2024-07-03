; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

; These macros are used to mark numbers as prime or non-prime. Since we are
; using `calloc', they will be initialized as valid.
%define VALID   0
%define INVALID 1

bits 32

section .data
    prime_fmt: db "%d is prime.", 0xA, 0x0

section .text
    extern printf:function
    extern calloc:function
    extern free:function

; void sieve(int iterations) {
;   buffer = calloc(iterations, sizeof(int));
;   for (i = 2; i < iterations; i++)
;     if (primes[i] == VALID)
;       printf("%d is prime.\n", i);
;
;       for (j = i; j < iterations; j += i)
;         primes[j] = INVALID;
;   free(buffer);
; }
global sieve
sieve:
    push    ebp
    mov     ebp, esp
    push    ebx

    ; Move first argument (iterations) into `edx', and call:
    ;   calloc(iterations, sizeof(int))
    ; The returned value will remain in `eax'. We need to pop `edx' and add 4 to
    ; `esp' instead of adding 8 directly because we want to preserve the number
    ;   of iterations in `edx'.
    mov     edx, [ebp + 8]
    push    4
    push    edx
    call    calloc
    pop     edx
    add     esp, 4

    ; Initialize the counter to 2, the first prime number
    mov     ecx, 2

.outer_loop:
    ; Break if we reached the target iterations
    cmp     ecx, edx
    jge     .outer_done

    ; If `primes[i]' is not marked as VALID, increment `i' and `continue'.
    cmp     [eax + ecx], byte VALID
    jne     .inner_done

    ; Before printing the number, make sure we preserve `eax' and `edx'
    push    eax
    push    edx

    ; Print the number by calling:
    ;   printf("%d is prime.\n", i);
    ; Just like before, we need to add 4 and pop `ecx' instead of adding 8
    ; because we want to preserve the iterator.
    push    ecx
    push    prime_fmt
    call    printf
    add     esp, 4
    pop     ecx

    ; Restore caller-preserved registers
    pop     edx
    pop     eax

    ; Initialize the counter of the inner loop (j) to the counter of the outer
    ; loop (i).
    mov     ebx, ecx

.inner_loop:
    ; Break if we reached the target iterations
    cmp     ebx, edx
    jge     .inner_done

    ; Mark the current element as non-prime (INVALID)
    mov     [eax + ebx], byte INVALID

    ; Add `ecx' to `ebx', effectively marking all primes divisible by `ecx' as
    ; INVALID.
    add     ebx, ecx
    jmp     .inner_loop

.inner_done:
    ; Increment the outer loop counter and continue
    inc     ecx
    jmp     .outer_loop

.outer_done:
    ; Free the array we allocated with calloc
    push    eax
    call    free
    add     esp, 4

    pop     ebx
    mov     esp, ebp
    pop     ebp
    ret
