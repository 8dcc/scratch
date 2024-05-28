; Kata: https://www.codewars.com/kata/52e88b39ffb6ac53a400022e
; FIXME

global uint32_to_ip
extern malloc

section .text

; RAX *itoa(RDI *str, RSI n)
itoa:
    push    rcx                 ; Preserve ecx

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
    mov     rax, rdi            ; return rdi;
    ret

; <--- char *uint32_to_ip(char *ip, uint32_t num) --->
uint32_to_ip:
    xor   rax, rax
    xor   rdx, rdx
    
    mov   rcx, 4        ; i = 4;
.loop:
    test  rcx, rcx      ; if (i == 0)
    jz    .done         ;   break;
    
    mov   rdx, rsi      ; Preserver rsi
    and   rsi, 0xFF     ; Last 8 bits
    call  itoa          ; Int to string
    mov   rsi, rdx      ; Restore rsi
    shr   rsi, 8

    dec   rcx           ; i--;
    jmp   .loop         ; continue;

.done:
    mov   [rdi], byte 0      ; Last '\0'
    ret

