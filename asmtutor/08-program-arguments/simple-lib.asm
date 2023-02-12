; -------------------------------------------------------------------------
; https://github.com/8dcc/asm-stuff
; This file contains some functions that will be used by another file when
; including. Keep in mind that the file might change depending on the
; current directory.
; -------------------------------------------------------------------------

; int slen(char*)
; Calculates length of string
slen:
    push    ebx                 ; We wil use ebx to store the pointer to the first character of out string
    mov     ebx,eax             ; eax contains the target string, move to ebx
slen_nextchar:
    cmp     byte[eax],0         ; Check end of string
    jz      slen_finished       ; Was end of string, go to finished
    inc     eax                 ; Else, next char
    jmp     slen_nextchar
slen_finished:
    sub     eax,ebx             ; eax points to the last position of the str, subtract the first pos to get len
    pop     ebx                 ; We don't need ebx anymore, pop from stack
    ret                         ; Return function

; void sprint(char*)
; Prints a string using the write syscall (4)
sprint:
    push    edx                 ; We push with reverse order to pop in the good one (it's not like it matters here but idk)
    push    ecx
    push    ebx
    push    eax                 ; Will be on top of the stack. eax is the char* we are going to use
    
    call    slen                ; We call slen to get the length of the str (needed for edx).
                                ; Returned value will be stored in eax but we have the original char* in stack

    mov     edx,eax             ; Move length to edx
    pop     eax                 ; pop the original eax (char*) from stack

    mov     ecx,eax             ; Move the char* to ecx
    mov     eax,4               ; Write
    mov     ebx,1               ; STDOUT
    int     0x80                ; Call kernel

    pop     ebx                 ; pop values back in reverse order from when we pushed
    pop     ecx
    pop     edx
    ret                         ; Return the function, eax was not pop'ed so it will be 4 (sys_write)

; void println(char*)
; Prints a string using the sprint function + '\n'
println:
    call    sprint

    push    eax                 ; Push current eax to stack to preserve it
    mov     eax, 0xA            ; Move '\n' to eax because we want to append it
    push    eax                 ; Push eax ('\n') to stack so we can get the address
    mov     eax, esp            ; esp points to the item on top of the stack ('\n'), so we move it to eax to use as function argument for sprint
    call    sprint              ; Print newline
    pop     eax                 ; Remove newline from stack
    pop     eax                 ; Get original eax from stack
    ret

; void printn(int)
; Prints a 1 digit int as char
; TODO: Print ints with multiple digits
printn:
    mov     ebp, esp            ; Save stack pointer

    push    0x0                 ; Null terminator for the str
    cmp     eax, 9              ; Check if it has 1 char
    jle     printn_s            ; If it has 1, jump to the end
    mov     eax, 15             ; If it has more, use 15: '?'(63) - '0'(48) = 15
printn_s:
    add     eax, 48             ; int -> char
    push    eax                 ; Push to stack so esp points to this
    mov     eax, esp            ; Move the address of the char to eax
    call    sprint              ; Call print with the char address as argument

    pop     eax                 ; Pop number char from stack
    mov     esp, ebp            ; Restore stack pointer
    ret

; void quit()
; Calls sys_exit (1)
quit:
    mov     eax,1               ; sys_exit
    mov     ebx,0               ; Return 0
    int     0x80                ; Call kernel (exit)
    ret                         ; Return should not be necesary but whatever
