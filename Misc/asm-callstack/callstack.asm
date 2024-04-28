; See: https://8dcc.github.io/reversing/understanding-call-stack.html

default rel
extern printf

section .data
    fmt: db "%d: %p", 0xA, 0x0

section .text

; void AsmPrintCallStack(int depth);
global AsmPrintCallStack
AsmPrintCallStack:
    push    rbp
    mov     rbp, rsp
    push    r12
    push    r13

    ; Initialize counter to the first "depth" argument
    mov     r13, rdi

    ; Get address of old RBP, skip ours
    mov     r12, [rbp]

.loop:
    ; If RBP is NULL, we are done.
    test    r13, r13
    jz      .done

    ; Otherwise, we can print it.
    ; We add 8 to the current RBP to get the current return address.
    ;   printf("%d: %p\n", i, cur_ret);
    mov     rdx, [r12 + 0x8]
    mov     rsi, r13
    lea     rdi, [rel fmt]
    call    printf wrt ..plt

    ; And jump to the next one
    ;   i--;
    ;   rbp = *rbp;
    ;   continue;
    dec     r13
    mov     r12, [r12]
    jmp     .loop

.done:
    pop     r13
    pop     r12
    mov     rsp, rbp
    pop     rbp
    ret
