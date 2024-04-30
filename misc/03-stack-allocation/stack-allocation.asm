; =====================================================================
; Warning, the comments on this asm file might be too awesome for you
; =====================================================================

section .text
    global _start

_start:
    mov     ebx, 123            ; Just a test value that will be stored in the stack
    push    ebx

    call    myfunc              ; Call my function

    pop     ebx                 ; Pop value from stack (should be 123 if the stack was unaltered)
    
    mov     eax, 1              ; Exit with ebx as return value
    int     0x80

; You can declare functions after the call
myfunc:
    ; Right now the next instruction from where we called is stored in the stack,
    ; so we could do sometihng like
    ;   pop eax
    ;   jmp eax
    ; Which is exactly what `ret` does

    mov     ebp, esp            ; We use the ebp register to store the current position of the stack
                                ; so we can restore it later into esp and have an unaltered stack

    sub     esp, 3              ; Because the top of the stack is in a "lower" position than the
                                ; bottom parts (pushes to a previous address in memory) we subtract
                                ; 3 from esp so we move that pointer to a previous position of the
                                ; stack, allocating 3 bytes. Current stack representation:
                                ; +---+---+---+---+---+---+---+---+---+
                                ; | ? |   |   |   |ret|123|...|...|...|
                                ; +---+---+---+---+---+---+---+---+---+
                                ;       ^           ^   ^ Sould be ebx from _start
                                ;       ^           ^ Pointer to next instruction after call (ebp now)
                                ;       ^ Stack pointer after instruction

    mov     [esp],   byte ':'   ; +-----+-----+-----+-----+-----+-----+-----+
    mov     [esp+1], byte '3'   ; |  ?  | ':' | '3' |'\n' | ret | 123 | ... |
    mov     [esp+2], byte 0xA   ; +-----+-----+-----+-----+-----+-----+-----+
                                ;          ^ esp             ^ ebp
                                ; Maybe it would be a good idea to add a '\0', but we will use `edx, 3`

    mov     eax, 4
    mov     ebx, 1
    mov     ecx, esp            ; Because esp is pointing to ':' we can just move that address to ecx
    mov     edx, 3
    int     0x80                ; Print

    ; Now keep in mind that esp is still pointing to ':' and we need esp to be the return address
    ; (next instruction after call, in this case `pop ebx`). That's why we stored esp into ebp
    mov     esp, ebp            ; Restore stack pointer
    ret                         ; We can safely return now

