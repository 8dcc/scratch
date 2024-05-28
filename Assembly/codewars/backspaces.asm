; Kata: https://www.codewars.com/kata/5727bb0fe81185ae62000ae3

section .text
    global strclr
    extern malloc

; [byte RAX] strclr(ro [byte RDI] s)
strclr:
    push    rdi                 ; rax = malloc(1000)
    xor     rax, rax
    mov     rdi, 1000
    call    malloc
    pop     rdi

    mov     rcx, rax            ; rcx = original allocated buf
    mov     rsi, rcx            ; rsi used to move the current buf ptr
    xor     rax, rax
    xor     rdx, rdx

.loop:
    mov     dl, byte [rdi]      ; dl = *str;
    test    dl, dl              ; if (*str == '\0')
    jz      .done               ;   break;

    inc     rdi                 ; str++;

    cmp     dl, 35              ; if (*str == '#')
    jne     .not_backspace

    cmp     rsi, rcx            ;   if (buf > original_buf)
    jle     .loop

    dec     rsi                 ;     buf--;
    jmp     .loop               ;     continue;

.not_backspace:
    mov     byte [rsi], dl      ; *buf = *str;
    inc     rsi                 ; buf++;
    jmp     .loop               ; continue;

.done:
    mov     [rsi], byte 0       ; *buf = '\0';
    mov     rax, rcx            ; return original_buf;
    ret

