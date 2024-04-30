%include "simple-lib.asm"

section .text
    global _start

_start:
    mov     ecx, 0      ; Will store the number to print, we start at 0

next_number:
    inc     ecx

    mov     eax, ecx    ; Move ecx to eax
    add     eax, '0'    ; int to ascii value
    push    eax         ; Push to stack so esp (the stack pointer) points to eax as string
    mov     eax, esp    ; Move stack pointer to eax
    call    println     ; Print the char* in eax, in this case esp, which points to the digit str

    pop     eax         ; pop the number we just printed from stack ('0', '1', ...)
    cmp     ecx, 10     ; Check if ecx (the original int iterator) reached 10 yet (we just printed it).
                        ; There is an error here because we are printing the (10 + '0') character, ':',
                        ; instead of '1' and then '0'.
    jne     next_number ; ecx != 10 -> next number

    call    quit        ; If we reached here it means ecx is 10 so we are finished
