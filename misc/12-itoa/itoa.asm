; FIXME: 64bit version doesn't return

; void itoa(EDI n, ESI *str)
itoa:
    push    edx                 ; Preserve edx

.loop:
    test    edi, edi            ; if (edi == 0)
    jnz     .done               ;   break;

    xor     edx, edx            ; Clear for remainder
    idiv    edi, 10             ; edx = edi % 10; eax = edi / 10;

    add     edx, '0'            ; Convert remainder to char
    mov     byte [esi], dl      ; Write byte to target str

    jmp     .loop               ; continue;

.done:
    pop     edx                 ; Restore edx

    ret

%ifdef BITS_64

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

%endif

