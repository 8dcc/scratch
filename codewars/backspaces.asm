; Kata: https://www.codewars.com/kata/5727bb0fe81185ae62000ae3
; FIXME

section .bss
  buf resb 1000

section .text
global strclr

; [byte RAX] strclr(ro [byte RDI] s)
strclr:
    xor     rdx, rdx
    mov     rsi, buf            ; rsi used for the buf location

.loop:
    mov     dl, byte [rdi]      ; dl = *str;
    test    dl, dl              ; if (*str == '\0')
    jz      .done               ;   break;

    cmp     dl, 35              ; if (*str == '#')
    jne     .not_backspace

    cmp     rsi, buf            ;   if (buf > original_buf) 
    jle     .loop

    dec     rsi                 ;     buf--;
    jmp     .loop               ;     continue;

.not_backspace:
    mov     [rsi], dl           ; *buf = *str;
    inc     rsi                 ; buf++;

    jmp     .loop               ; continue;

.done:
    mov     [rsi], byte 0       ; *buf = '\0';
    mov     rax, buf            ; return original_buf;
    ret

