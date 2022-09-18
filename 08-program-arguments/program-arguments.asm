; Simple program that iterates and prints the provided arguments
; Arg number only works if they are less than 10 args

%include 'simple-lib.asm'

section .data
    tab_ptr: db 0x9         ; '\t'

section .text
    global _start

_start:
    pop     ecx             ; Save the value on top of the stack in ecx.
                            ; This is the number of arguments.

loop_args:
    cmp     ecx, 0x0        ; First, check if there are args left
    jz      no_args_left    ; If there are no args left, break

    ; Print ecx number + '\t' before arg itself
    mov     eax, ecx        ; Move the current iteration to eax
    call    printn          ; Call printn(int) to print the char
    mov     eax, tab_ptr    ; Print tab char
    call    sprint

    pop     eax             ; Save argv[ecx] into eax
    call    println         ; Print eax (Keep in mind argv is a char**)
    dec     ecx             ; Decrease arg number
    jmp     loop_args

no_args_left:
    call    quit            ; We done
