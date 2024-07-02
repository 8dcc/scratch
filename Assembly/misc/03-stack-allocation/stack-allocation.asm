
section .text
    global _start

_start:
    mov     ebx, 123
    push    ebx

    ; Call my function that should not modify the stack frame
    call    myfunc

    ; Pop value from stack (should be 123 if the stack was unaltered)
    pop     ebx

    ; Exit with `ebx' as return value
    mov     eax, 1
    int     0x80

myfunc:
    ; When entering the function, the return address of the caller is on the top
    ; of the stack. The `ret' instruction would "pop it" and jump to it.
    ;
    ; After entering, we push `ebp' for preserving the initial value of `esp'.
    ;   +---+---+---+---+---+---+---+---+---+---+
    ;   | ? |   |   |   |rbp|ret|123|...|...|...|
    ;   +---+---+---+---+---+---+---+---+---+---+
    ;                             ^ Value pushed in _start
    ;                     ^ ebp/esp
    push    ebp
    mov     ebp, esp

    ; Because the top of the stack is in a lower memory address, we subtract 3
    ; from `esp' so we move that pointer to a previous position of the stack,
    ; allocating 3 bytes.
    ;   +---+---+---+---+---+---+---+---+---+---+
    ;   | ? |   |   |   |rbp|ret|123|...|...|...|
    ;   +---+---+---+---+---+---+---+---+---+---+
    ;         ^ esp after instruction
    sub     esp, 3

    ; We write the 3 bytes. We don't need a null-terminator in this case since
    ; we know we are going to be writing exactly 3 bytes.
    ;   +---+---+---+---+---+---+---+---+---+---+
    ;   | ? |':'|')'|0xA|rbp|ret|123|...|...|...|
    ;   +---+---+---+---+---+---+---+---+---+---+
    mov     [esp],   byte ':'
    mov     [esp+1], byte ')'
    mov     [esp+2], byte 0xA

    ; Since esp is pointing to the string we can use it directly when calling
    ; SYS_WRITE.
    mov     edx, 3
    mov     ecx, esp
    mov     ebx, 1              ; STDOUT
    mov     eax, 4              ; SYS_WRITE
    int     0x80

    ; We could simply add 3 to `esp', but since we preserved the stack frame, we
    ; can just restore `ebp' and return.
    mov     esp, ebp
    pop     ebp
    ret
