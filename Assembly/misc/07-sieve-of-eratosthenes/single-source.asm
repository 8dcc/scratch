; nasm -f elf32 -o a.o a.asm
; ld -m elf_i386 -o a.out a.o
%define NUM 2000
bits 32
section .bss
primes: resb NUM
section .text
global _start
printi:
push eax
push ebx
push ecx
push edx
push ebp
mov ebp,esp
dec esp
mov [esp],byte 0
mov esi,10
.loop:
dec esp
mov edx,0
idiv esi
add edx,'0'
mov [esp],dl
cmp eax,0
jnz .loop
.done:
mov edx,ebp
sub edx,esp
mov ecx,esp
mov ebx,1
mov eax,4
int 0x80
mov esp,ebp
pop ebp
pop edx
pop ecx
pop ebx
pop eax
ret
printc:
push edx
push ecx
push ebx
push eax
mov edx,1
mov ecx,esp
mov ebx,1
mov eax,4
int 0x80
pop eax
pop ebx
pop ecx
pop edx
ret
puti:
call printi
push eax
mov eax,0x20
call printc
pop eax
ret
_start:
mov ecx,2
.outer_loop:
cmp ecx,NUM
jge .outer_done
cmp byte [primes+ecx],0
jne .inner_done
mov eax,ecx
call puti
mov ebx,ecx
.inner_loop:
cmp ebx,NUM
jge .inner_done
mov byte [primes+ebx],1
add ebx,ecx
jmp .inner_loop
.inner_done:
inc ecx
jmp .outer_loop
.outer_done:
mov eax,1
mov ebx,0
int 0x80
