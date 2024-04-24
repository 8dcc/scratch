

section .text

; rax -> 8-byte (qword)
; eax -> 4-byte (dword)
;  ax -> 2-byte (word)
;  al -> 1-byte (byte)

; rax memcpy_asm(void* rdi, const void* rsi, size_t rdx);
global memcpy_asm
memcpy_asm:
    mov         ecx, edx
    rep movsb
    mov         rax, rdi
    ret
