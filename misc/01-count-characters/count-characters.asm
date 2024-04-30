
section .data
    mainStr     db  "Testing...", 0xA                   ; String that will be counted. In this program needs to be less than 10
    charStr     db  "Total characters: ", 0x0           ; Just for output formatting
    charStrLen: equ $-charStr                           ; See hello-world

section .text
    global _start

_start:
    mov     ebx, mainStr        ; Move address of mainStr to ebx
    mov     eax, ebx            ; Move the same address that is in ebx to eax as well

; ---------------------------------------------------
; Start of print
nextchar:                       ; We define a new label that will be called after each character that eax is pointing to is not '\0'
    cmp     byte[eax], 0        ; Compare the current byte in eax with '\0'
    jz      finished            ; Jump to 'finished' if the previous condition was true
    inc     eax                 ; If we are here is because we didn't jump (condition was false), so we increment eax to point to next char
    jmp     nextchar            ; Go back to checking the changed pointer

finished:                       ; We will jump here when the character is '\0' (String ended)
    sub     eax, ebx            ; Now eax is pointing to the last char of mainStr, but ebx is still pointing to the first char. We subtract
                                ; the 2 pointers (pointer arithmetic) to get the number of chars we incremented (length). This subtracted
                                ; value will be stored in eax.
    mov     edx, eax            ; Move the current eax value to edx because it indicates the length number to the function
    
    mov     eax, 4              ; Now that the value has been moved to edx we can use eax normally to call sys_write
    mov     ebx, 1              ; STDOUT
    mov     ecx, mainStr
    int     0x80
; End of print
; ---------------------------------------------------

    ; Print charStr
    push    edx                 ; Push to stack because we need to print text first and use this register
    mov     edx, charStrLen     ; We can use edx safely now
    mov     ecx, charStr        ; We don't need to move eax and ebx again
    int     0x80                ; Print "Total characters: "
    ; -- Stops printing here -- 

    pop     edx                 ; Get the value back
    add     edx, 48             ; Convert int to char (Only 1 char so it has to be < 10)
    push    edx                 ; Push the converted char value to stack so esp points to it
    mov     ecx, esp            ; esp now has our "string", so use that as parameter
    mov     edx, 1              ; edx is still on the stack, but now we use it as normal parameter
    int     0x80                ; Print character itself

    mov     eax, 1
    mov     ebx, 0
    int     0x80                ; Exit

