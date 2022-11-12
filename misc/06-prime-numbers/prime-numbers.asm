
%include 'simple-lib.asm'

section .data
    s_args  db  "Not enough arguments, I need a number.", 0x0
    s_prime db  " is prime", 0x0

section .text
    global _start

_start:
    pop     ecx             ; Save the value on top of the stack in ecx
                            ; This is the number of arguments
    cmp     ecx, 1          ; argc <= 1
    jle     arg_die

    mov     ecx, 1          ; Set ecx to 1, we will be using ecx to loop each number we want to check

    pop     edx             ; First arg is the program name, ignore.
    pop     edx             ; Seccond arg should be the number
    mov     eax, edx        ; Move to eax for atoi
    call    atoi            ; Convert string argument to int
    mov     edx, eax        ; Move the converted int to edx, we will be using edx to check against ecx when looping

.num_loop:
    cmp     ecx, edx        ; First, check if we are done with all the numbers
    jg      .num_done       ; (ecx > edx) If we checked all the numbers we wanted, break

    mov     eax, ecx        ; Move current number to eax to use as argument for isprime()
    call    isprime         ; Check if the current number is prime
    
    cmp     eax, 0          ; If it's not prime, don't print anything
    jz      .not_prime
    mov     eax, ecx
    call    iprint          ; Print the prime number
    mov     eax, s_prime    ; " is prime"
    call    println

.not_prime:
    inc     ecx             ; (i++) Increase current number
    jmp     .num_loop       ; continue

.num_done:
    call    quit            ; We are done

; ----------------------------------------------------------------

; int isprime(int n)
; Returns true (1) if the number is prime, false (0) if it is not
isprime:
    push    ebx
    push    ecx             ; Save ecx for iteration
    push    edx

    push    eax
    xor     edx, edx        ; For division remainder. Not really needed
    mov     ebx, 2
    div     ebx             ; eax /= 2
    mov     ebx, eax        ; Save half of original eax into ebx (for the loop)
    pop     eax

    ; for (ecx = 2; ecx <= eax / 2; ecx++) if (ecx % eax == 0) return 0;
    mov     ecx, 2

.loop:
    cmp     ecx, ebx        ; Check if ecx is greater than half of the original eax. We loop like this because n/2 is the biggest possible divisor
    jg      .prime          ; if (ecx > eax/2) we are done with the loop and there were no divisors, we can return that the number is prime

    push    eax             ; Number we want to check
    xor     edx, edx        ; Clear edx for division remainder
    div     ecx             ; edx = eax % ecx; eax /= ecx;
    pop     eax             ; Restore original number for next iteration

    cmp     edx, 0          ; eax is divisible by the current iteration, not prime
    jz      .not_prime

    inc     ecx             ; (i++) Increase divisor check
    jmp     .loop

.prime:
    mov     eax, 1          ; return 1
    jmp     .done

.not_prime:
    mov     eax, 0          ; return 0
    jmp     .done

.done:
    pop     edx             ; Restore used registers
    pop     ecx
    pop     ebx
    ret

; ----------------------------------------------------------------

arg_die:
    mov     eax, s_args     ; "Not enough arguments, I need a number."
    call    println

    mov     eax, 1          ; sys_exit
    mov     ebx, 1          ; exit code
    int     0x80
