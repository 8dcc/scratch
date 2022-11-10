
%include 'simple-lib.asm'

section .text
    global _start

_start:
    pop     ecx             ; Save the value on top of the stack in ecx.
                            ; This is the number of arguments.

    pop     edx             ; First arg is the program name, ignore.
    xor     edx, edx        ; Set edx to 0. We will be using edx (the data register) for the final result.
    dec     ecx             ; Decrease the argument number by 1

.arg_loop:
    cmp     ecx, 0x0        ; First, check if there are args left
    jz      .arg_done       ; If there are no args left, break

    pop     eax             ; Save argv[ecx] into eax
    call    atoi            ; String to int
    add     edx, eax        ; Add converted int to edx (total)

    dec     ecx             ; Decrease arg number
    jmp     .arg_loop       ; continue

.arg_done:
    mov     eax, edx        ; Final result to eax for argument
    call    iprintln         ; Print final result

    call    quit            ; We are done
