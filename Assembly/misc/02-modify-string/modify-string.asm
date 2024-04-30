section .data
    msg1        db  "Hello world!", 0xA, 0x0    ; str + '\n' + '\0'
    msg1_len    equ $-msg1                      ; cur pointer pos - msg1 pointer pos

section .text
    global _start

print_msg:                          ; Create label that will be called as func to print msg1
    mov     eax, 4                  ; sys_write
    mov     ebx, 1                  ; stdout
    mov     ecx, msg1               ; msg1 pointer
    mov     edx, msg1_len           ; Lenght stored in variable
    int     0x80                    ; Kernel interrupt
    ret                             ; Return function

exit:                               ; Didn't want to add the "library" from the other folders
    mov     eax, 1
    mov     ebx, 0
    int     0x80
    ret                             ; Should never reach here anyway

_start:
    call    print_msg               ; Call function to print the orignal message

    mov     [msg1+7], byte '0'      ; Add 7 to msg1 pointer to access the letter 'o', then move the new char with
                                    ; byte type (same as C char). Also the char can be inside '' but it just acts
                                    ; like in C, using its numerical value.
    call    print_msg               ; Call function again to print changes

    call    exit                    ; Exit with exit code 0

