; https://www.codewars.com/kata/5865918c6b569962950002a1/train/nasm
; int32_t str_count(const char *str, char letter);

section .text
global str_count

str_count:
  xor   rax, rax
  xor   rdx, rdx
  
.loop:
  mov   dl, byte [rdi]    ; dl = *arg1
  test  dl, dl            ; *arg1 == '\0'
  jz    .done             ; break

  cmp   rdx, rsi          ; *arg1 != arg2
  jne   .continue         ; continue

  inc   rax               ; ret++

.continue:
  inc   rdi               ; arg1++
  jmp   .loop

.done:
  ret
