; Simple program that iterates and prints the provided arguments

%include 'simple-lib.asm'

section .text
    global _start

_start:
    pop     ecx             ; Save the value on top of the stack in ecx.
                            ; This is the number of arguments.

loop_args:
    cmp     ecx, 0x0        ; First, check if there are args left
    jz      no_args_left    ; If there are no args left, break
    pop     eax             ; Save argv[ecx] into eax
    call    println         ; Print eax (Keep in mind argv is a char**)
    dec     ecx             ; Decrease arg number
    jmp     loop_args

no_args_left:
    call    quit            ; We done
