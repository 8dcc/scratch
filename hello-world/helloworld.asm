; For more information about asm:
;   http://asm.sourceforge.net
;   http://asm.sourceforge.net/howto/hello.html
;   https://asmtutor.com
; For more information about syscalls:
;   http://www.lxhp.in-berlin.de/lhpsyscal.html

section .data
    helloMsg: db "Hello world!", 0xA        ; 0xA = '\n'
    helloLen: equ $-helloMsg                ; Subtract the helloMsg pointer position from the current position,
                                            ; then asign it to helloLen using equ (for constant)

section .text
    global _start

_start:
    mov eax,4                               ; sys_write syscall (4)
    mov ebx,1                               ; File handle code (STDOUT)
    mov ecx,helloMsg                        ; helloMsg pointer as 2nd argument
    mov edx,helloLen                        ; String length from the .data section
    int 0x80                                ; Interrupt with code 0x80 (call kernel)

    mov eax,1                               ; sys_exit syscall (1)
    mov ebx,0                               ; Exit code (return 0)
    int 0x80                                ; Call kernel again
