section .data
    hello    db  "lmao stfu", 0xA

section .text
    global _start

_start:
    mov     eax,hello       ; eax points to hello
    call    strlen          ; Call the function named strlen

    mov     edx,eax         ; eax now holds the len, move to actual function argument we want (edx)
    mov     eax,4           ; Same as ../hello-world
    mov     ebx,1
    mov     ecx,hello
    int     0x80            ; Print

    mov     eax,1
    mov     ebx,0
    int     0x80            ; Exit

strlen:
    push    ebx             ; Push to stack so we can pop after using it in the func
    mov     ebx,eax         ; Make both registers point to the same place (hello)

nextchar:
    cmp     byte[eax],0     ; Compare current char with '\0'
    jz      finished        ; If true, jump to 'finished'
    inc     eax             ; Else, increment eax and loop again
    jmp     nextchar

finished:
    sub     eax,ebx         ; Get the length and store it in eax
    pop     ebx             ; We don't need ebx so we pop, restoring its previous value
    ret                     ; We return to where the function was called
