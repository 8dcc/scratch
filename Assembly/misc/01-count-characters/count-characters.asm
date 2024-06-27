
section .data
    ; String that will be counted. Needs to be less than 10 characters.
    mainStr:    db  "Testing!", 0xA, 0x0

    charStr:    db  "Total characters: "    ; Just for output formatting
    charStrLen: equ $-charStr               ; See hello-world

section .text
    global _start

_start:
    ; Save the address of `mainStr' to `ebx' and `eax'
    mov     ebx, mainStr
    mov     eax, ebx

    ; Increment `eax' until we reach '\0'. This is not the best way of looping,
    ; see: scratch/C/misc/bf2nasm/branching.pdf
.loop:
    cmp     byte [eax], 0
    jz      .done

    inc     eax
    jmp     .loop

.done:
    sub     eax, ebx            ; Store address difference in `eax'

    ; Preserve `eax' (the length of `mainStr')
    push    eax

    ; Print mainStr
    mov     edx, eax            ; Number of bytes to write
    mov     ecx, mainStr        ; The string to write
    mov     ebx, 1              ; Output: STDOUT
    mov     eax, 4              ; Invoke SYS_WRITE
    int     0x80

    ; Print charStr
    mov     edx, charStrLen
    mov     ecx, charStr        ; "Total characters: "
    mov     ebx, 1
    mov     eax, 4
    int     0x80

    ; Restore the value we pushed
    pop     eax

    ; Convert digit in `eax' to char (3 -> '3'). This limits the string length
    ; to 9 characters, but it's good enough to illustrate the program.
    add     eax, 48

    ; Store a newline in `ebx', shift it 8 bits to the left so it's placed in
    ; the second byte (bits 8..15) of `ebx'. Then OR it together with our digit
    ; character in `eax', effectively creating a two byte string: "9\n".
    mov     ebx, 0xA
    shl     ebx, 8
    or      eax, ebx

    ; Push the DWORD containing the converted char value and the newline to the
    ; stack so we can use `esp' as a pointer to our "string".
    push    eax

    ; Print the character containing the length of `mainStr'
    mov     edx, 2              ; We are printing a newline and a character
    mov     ecx, esp            ; `esp' points to our "string"
    mov     ebx, 1
    mov     eax, 4
    int     0x80

    ; Pop the value we used for printing. We could also add `sizeof(void*)' to
    ; `esp', since we don't care about the value itself.
    pop     eax

    ; Exit the program
    mov     ebx, 0
    mov     eax, 1
    int     0x80
