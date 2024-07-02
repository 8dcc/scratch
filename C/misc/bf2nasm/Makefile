
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -DLINUX_SYSCALLS

ASM=nasm
ASMFLAGS=-f elf64

LD=ld
LDFLAGS=-m elf_x86_64

#-------------------------------------------------------------------------------

.PHONY: all clean

all: output.out

clean:
	rm -f output.out output.o output.asm bf2nasm.out

#-------------------------------------------------------------------------------

# NOTE: Change depending if LINUX_SYSCALLS was defined or not
output.out: output.o
#	$(CC) -o $@ $^
	$(LD) $(LDFLAGS) -o $@ $^

output.o: output.asm
	$(ASM) $(ASMFLAGS) -o $@ $^

output.asm: bf2nasm.out input.bf
	./bf2nasm.out < input.bf > output.asm

bf2nasm.out: bf2nasm.c
	$(CC) $(CFLAGS) -o $@ $^
