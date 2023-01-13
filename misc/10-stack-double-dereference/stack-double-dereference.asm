
; Meant for debugging with gdb

section .data:
    sample  db "Hello!", 0x0

section .text:
    global _start

_start:
    mov     eax, sample
    push    eax             ; 2nd arg
    push    123             ; 1st arg
    call    func

    mov     ebx, 0
    mov     eax, 1
    int     0x80

; void func(int a, char* b)
func:
    ; ESP = [ RET_ADDR,   a, b ]
    ; ESP = [ 0x125612, 123, 0xABC123 ("LOL") ]

    ; eax = *(esp + 4) = a = 123
    mov     eax, [esp + 4]

    ; ebx = *(esp + 8) = b (char*) = 0xABC123
    mov     ebx, [esp + 8]

    ; ecx = *(*(esp + 8)) = *b = 'L'
    mov     ecx, [ebx]

    ret

