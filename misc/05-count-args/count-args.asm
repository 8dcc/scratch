; Simple program that iterates and prints the provided arguments
; Arg number only works if they are less than 10 args

%include "simple-lib.asm"

section .data
    tab_ptr db 0x9         ; '\t'

section .text
    global _start

_start:
    pop     edx             ; Save the value on top of the stack in ecx.
                            ; This is the number of arguments (argc).

    mov     ecx, 0          ; Same as "xor ecx, ecx"

.loop:
    cmp     ecx, edx        ; First, check if we printed all args
    jge      .done          ; If there are no args left, break (edx >= ecx)

    ; Print ecx number + '\t' before arg itself
    mov     eax, ecx        ; Move the current iteration to eax
    call    printi          ; Call printn(int) to print the char
    mov     eax, tab_ptr    ; Print tab char
    call    prints

    pop     eax             ; Save argv[ecx] into eax
    call    puts            ; Print eax (Keep in mind argv is a char**)
    inc     ecx             ; Increase current arg count
    jmp     .loop

.done:
    call    quit            ; We done
