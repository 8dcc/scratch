; -------------------------------------------------------------------------------
; https://github.com/r4v10l1/asm-stuff
; This file contains functions that will be used by another file when including
; -------------------------------------------------------------------------------

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
    
    call    slen                ; We call slen to get the length of the str (needed for edx). Returned value will be stored in eax but we have the original char* in stack
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

; void quit()
; Calls sys_exit (1)
quit:
    mov     eax,1               ; sys_exit
    mov     ebx,0               ; Return 0
    int     0x80                ; Call kernel (exit)
    ret                         ; Return should not be necesary but whatever
