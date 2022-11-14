
%include "simple-lib.asm"

section .data
    cmd     db      "/bin/echo", 0x0        ; We define the bytes (strings) with the commands and null terminators
    arg1    db      "I am the arg!", 0x0    ; Same for each argument, we need to pass an array of pointers, not strings

    args    dd      cmd         ; We define the argument struct/ptr array. Notice we are declaring *double words* and not bytes,
            dd      arg1        ; because the struct contains pointers to each str.
            dd      0x0         ; It contains the cmd path, each arg, and a null terminator for the end of the struct.

    env     dd      0x0         ; Arguments to pass as enviroment variables, in this case none, but (afaik) the struct should be
                                ; declared like the args struct/array


section .text
    global _start

_start:
    mov     edx, env
    mov     ecx, args           ; Also contain the cmd as first ptr
    mov     ebx, cmd
    mov     eax, 11             ; sys_execve, the syscall name format for execs is exec[E,L,P,V] where each letter means:
                                ;   E - An array of pointers to environment variables is explicitly passed to the new process image.
                                ;   L - Command-line arguments are passed individually to the function.
                                ;   P - Uses the PATH environment variable to find the file named in the path argument to be executed.
                                ;   V - Command-line arguments are passed to the function as an array of pointers.
                                ; so in this case we are passing the enviroment struct (edx), the arguments (ecx), and the cmd itself (ebx)
    
    int     0x80

    call    quit
