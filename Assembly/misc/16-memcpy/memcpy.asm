
section .text

; rax memcpy_asm(void* rdi, const void* rsi, size_t rdx);
global memcpy_asm
memcpy_asm:
    mov         ecx, edx
    rep movsb
    mov         rax, rdi
    ret
