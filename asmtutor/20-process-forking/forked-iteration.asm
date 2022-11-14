; Made to show process forking when printing

%include "simple-lib.asm"

section .data
    test_s  db  "Testing process forking...", 0x0
    paren   db  "()"    ; Used by ten_iter to surround each number, we don't need null terminator because it's just a:
    brack   db  "[]"    ; char arr[2]

section .text
    global _start

_start:
    mov     eax, test_s ; "Testing process forking..."
    call    println

    mov     eax, 2      ; sys_fork (kernel opcode 2)
    int     0x80
    
    cmp     eax, 0      ; From here the 2 processes will continue, we can check what process we are in by checking eax,
                        ; so one process executes something and the child, another part of the code.
                        ; eax == 0 means we are in the child process
    jz      child

parent:
    mov     eax, 10
    mov     ebx, paren
    call    ten_iter
    
    mov     eax, 0xA    ; '\n'
    call    cprint

    call    quit

child:
    mov     eax, 20
    mov     ebx, brack
    call    ten_iter

    mov     eax, 0xA    ; '\n'
    call    cprint

    call    quit

; void ten_iter(int n, char c[2])
; Will iterate and print from eax to eax + 10 (and with a space)
; It will print c[0] and c[1] surrounding each number
ten_iter:
    push    eax
    push    ecx
    
    mov     ecx, eax        ; Copy eax to ebx, this will be our starting point in the loop
    add     eax, 10         ; (eax += 10) Our target

.loop:                      ; for (int i = eax; i <= eax + 10; i++) printf(i + " ");
    cmp     ecx, eax        ; if (ecx > eax)
    jg      .done           ; break;

    push    eax
    mov     eax, [ebx]      ; Dereference the char* from ebx to get the first char, and print it ('[')
    call    cprint
    mov     eax, ecx        ; Print the current iteration
    call    iprint
    mov     eax, [ebx+1]    ; Dereference ebx[1] to get the second char, and print it (']')
    call    cprint
    ; mov     eax, 0x20       ; ' '
    ; call    cprint
    pop     eax

    inc     ecx             ; i++
    jmp     .loop           ; continue;

.done:
    pop     ecx
    pop     eax
    ret
