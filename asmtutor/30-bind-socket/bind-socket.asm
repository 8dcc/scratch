
%include "simple-lib.asm"
%include "../syscalls.asm"

section .text
    global _start

_start:
    xor     eax, eax        ; Set registers to 0
    xor     ebx, ebx
    xor     ecx, ecx
    xor     edx, edx

.socket:
    push    byte 6          ; Same as ../29-*
    push    byte 1
    push    byte 2
    mov     ecx, esp
    mov     ebx, 1
    mov     eax, sys_socketcall
    int     0x80

.bind:
    mov     edi, eax        ; Save FD we got when creating the socket in edi
    push    dword 0x0       ; (4 BYTE PADDING) Figured out why the arg size is 16. See:
                            ; https://www.scip.ch/en/?labs.20200521 (Bind the socket part)
    push    dword 0x0       ; (4 BYTE PADDING)
    push    dword 0x0       ; (IP ADDRESS) Push the double word (32bit = 4bytes) 0 into the stack (0.0.0.0)
    push    word 0x2923     ; (PORT) 9001 dec = 0x2329 hex. Because we are pushing we need to reverse the bytes.
                            ; Important note: In x86 asm, a dword is 32bits and a word is 16bits! So reversing
                            ; the word like that makes sense.
    push    word 2          ; (AF_INET)
    mov     ecx, esp        ; Argument pointer into ecx
    push    byte 16         ; Size of the arguments ecx is pointing to. See stack comment bellow
    push    ecx             ; Push the arg pointer
    push    edi             ; Push the FD from the socket
    mov     ecx, esp        ; Current stack layout:
                            ; ...               | Bottom of the stack (Bigger address in memory because stack goes down)
                            ; 0x00000000        | Padding 1
                            ; 0x00000000        | Padding 2
                            ; 0x00000000        | IP ADDRESS
                            ; 0x2923            | PORT
                            ; 0X0002      <--   | AF_INET
                            ; 0x10 (16)     |   | Size of arguments we just pushed. (4 + 4 + 4 + 2 + 2)
                            ; ptr -----------   | Pointer to args
                            ; FD                | File descriptor we got from last sys_socketcall. We move this address to ecx
                            ; ...               | Top of the stack (Lower address in memory)
    mov     ebx, 2          ; Subroutine bind (2)
    mov     eax, sys_socketcall
    int     0x80

.exit:
    call    quit
