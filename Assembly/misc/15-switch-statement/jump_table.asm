; For a more detailed explanation of this code, see:
;   https://8dcc.github.io/programming/switch-statement.html
;
;-------------------------------------------------------------------------------
;
; This code defines a `my_switch_test' function, that uses a jump table to
; return a value depending on its input. This is what a compiler might generate
; when using `switch' statements in a language like C.
;
; This code illustrates why the values in the `case' statements need to be known
; at compile-time.
;
; Calling the function like this:
;
;     int result = my_switch_test(x);
;
; Would be equivalent to calling this C function:
;
;     int result;
;     switch (x) {
;         case 3:  result = 0x30;  break;
;         case 4:  result = 0x40;  break;
;         case 5:  result = 0x50;  break;
;         case 9:  result = 0x90;  break;
;         default: result = 0x100; break;
;     }
;     return result;
;
; Note how `result' isn't returned immediately from inside the `switch'
; statement. This is to emphasize that neither the body of each `case' nor the
; code after the `switch' statement are relevant to the logic of our jump table.
;
;-------------------------------------------------------------------------------
;
; The jump table method in this function assumes that all the values in the
; `case' labels are more or less consecutive. To support non-consecutive values,
; we simply use the address of the `case_default' in those indexes of the jump
; table.
;
; For example, the indexes of the jump table for the 6, 7 and 8 values all point
; to the same label as the `default' case.
;
; This method might not be practical for big or complex `switch' statements.
;
;-------------------------------------------------------------------------------
;
; Since we are writing 64-bit assembly, we multiply the index of the jump table
; by the size of each element, in this case 8 for `void*'. For example:
;
;   int arr[] = /* ... */;
;   int elem  = arr[i];
;
; Is equal to the following C code:
;
;   int arr[]     = /* ... */;
;   uintptr_t ptr = (uintptr_t)arr;
;   int elem      = ptr + (i * sizeof(int));
;
; The intermediate variable `ptr' is used to emphasize that `arr[i]' is the
; exact same as `arr+i', since indexing (and pointer addition) implicitly
; multiplies `i' by the size of each element of the array.
;
; You might also see that compilers optimize multiplications by powers of 2 with
; left bit shifts (shl). This will be used below.
;
;-------------------------------------------------------------------------------

default rel
bits 64

section .data
jump_table:
    dq case_3
    dq case_4
    dq case_5
    dq case_default ; case 6, not specified
    dq case_default ; case 7, not specified
    dq case_default ; case 8, not specified
    dq case_9
    ; Cases greater than 9 are handled manually below

section .text

; int my_switch_test(int x);
global my_switch_test
my_switch_test:
    push    rbp
    mov     rbp, rsp

    ; First of all, make sure we are within the bounds of the jump table. In
    ; other words, the parameter smaller than the smallest case, or if it's
    ; greater than the biggest one. If it is, jump to the `default' case.
    cmp     rdi, 3
    jl      case_default
    cmp     rdi, 9
    jg      case_default

    ; First, subtract the lowest value of the switch statement. In this case,
    ; 3. The first parameter is in `rdi'.
    sub     rdi, 3

    ; Save the address of the jump table in the `rdx' register. For more
    ; information about why we use `lea' instead of `mov', see:
    ; https://8dcc.github.io/reversing/understanding-call-stack.html#note-about-position-independent-executables
    lea     rdx, [jump_table]

    ; Multiply the jump table index by the size of each element, in this case
    ; 8. Note that multiplying by 8 is the same as shifting 3 bits to the left.
    shl     rdi, 3

    ; Add the index of the jump table (rdi) to the base address of the jump
    ; table (rdx).
    add     rdx, rdi

    ; Get the address of the label we should jump to from the jump table.
    mov     rdx, [rdx]

    ; Jump to that label.
    jmp     rdx

switch_done:
    ; This is the code that would be executed after we `break' out of the
    ; `switch'. It's not a local label because it has to be accessed by each
    ; `case'. In this case, we can move the value in `rdi' to `rax' to return
    ; it.
    mov     rax, rdi

    mov     rsp, rbp
    pop     rbp
    ret


; Cases for the user-specified values, known at compile-time. The address of
; these labels will be stored in the jump table.
case_3:
    mov     rdi, 0x30
    jmp     switch_done

case_4:
    mov     rdi, 0x40
    jmp     switch_done

case_5:
    mov     rdi, 0x50
    jmp     switch_done

case_9:
    mov     rdi, 0x90
    jmp     switch_done

; Default case, executed when the value was not specified in the `switch'.
case_default:
    ; In this case, just assign 0x100 and break.
    mov     rdi, 0x100
    jmp     switch_done
