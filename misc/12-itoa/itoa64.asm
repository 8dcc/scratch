global itoa

section .text

; RAX *itoa(RDI *str, RSI n)
itoa:
    push    rcx                 ; Preserve ecx
    push    rdx                 ; Preserve edx

    mov     rcx, 10
.loop:
    test    rsi, rsi            ; if (rsi == 0)
    jz      .done               ;   break;

    xor     rdx, rdx            ; Clear for remainder

    mov     rax, rsi            ; rax = rsi;
    div     rcx                 ; rdx = rax % 10; rax /= 10;
    mov     rsi, rax            ; rsi = rax;

    add     rdx, '0'            ; Convert remainder to char
    mov     byte [rdi], dl      ; Write byte to target str
    inc     rdi

    jmp     .loop               ; continue;

.done:
    pop     rdx                 ; Restore edx
    pop     rcx                 ; Restore ecx
    mov     rax, rdi            ; return rdi;
    ret

