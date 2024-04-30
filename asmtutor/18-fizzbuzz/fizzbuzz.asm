; Used my own method checking 15, 5 and 3

%include "simple-lib.asm"

section .data
    args        db  "Not enough arguments, I need a number!", 0x0
    s_fizz      db  "Fizz", 0x0
    s_buzz      db  "Buzz", 0x0
    s_fizzbuzz  db  "FizzBuzz", 0x0

section .text
    global _start

_start:
    pop     ecx         ; argc
    cmp     ecx, 1      ; argc <= 1
    jbe     arg_die

    pop     eax         ; argv[0], program name
    pop     eax         ; argv[1], fizzbuzz number
    call    atoi        ; eax = atoi(argv[1])
    mov     ebx, eax    ; ebx now contains the ammount of numbers we want to fizzbuzz
    xor     ecx, ecx    ; Clear ecx (old argc)

.loop:
    cmp     ecx, ebx
    jge     .done

    inc     ecx         ; We start at 0 and the first number we want to check is 1
    mov     eax, ecx
    call    fizzbuzz_chk

    jmp     .loop

.done:
    call    quit

; ------------------------------------------------------------------

arg_die:
    mov     eax, args   ; "Not enough arguments, I need a number!"
    call    println

    mov     eax, 1      ; sys_exit
    mov     ebx, 1      ; Return 1
    int     0x80

; ------------------------------------------------------------------

; void fizzbuzz_chk(int n)
; Checks if eax is divisible by 15, 5 or 3, and prints each string depending on the case
fizzbuzz_chk:
    push    eax
    push    ebx
    push    ecx
    push    edx
    mov     ebx, eax    ; Save eax so we can return it in case its not fizzbuzz

    xor     edx, edx    ; edx = 0
    mov     ecx, 15
    div     ecx         ; edx = eax % 15; eax /= 15
    cmp     edx, 0
    jz      .fizzbuzz

    xor     edx, edx
    mov     eax, ebx
    mov     ecx, 5
    div     ecx         ; edx = eax % 5; eax /= 5
    cmp     edx, 0
    jz      .buzz

    xor     edx, edx
    mov     eax, ebx
    mov     ecx, 3
    div     ecx         ; edx = eax % 3; eax /= 3
    cmp     edx, 0
    jz      .fizz
    
    mov     eax, ebx    ; No matches, print original number
    call    iprintln

    jmp     .done

.fizzbuzz:
    mov     eax, s_fizzbuzz     ; "FizzBuzz"
    call    println
    jmp     .done

.buzz:
    mov     eax, s_buzz         ; "Buzz"
    call    println
    jmp     .done

.fizz:
    mov     eax, s_fizz         ; "Fizz"
    call    println
    jmp     .done

.done:
    pop     edx
    pop     ecx
    pop     ebx
    pop     eax
    ret
    
