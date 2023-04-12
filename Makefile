
# CC used for tests
CC=gcc
CLFAGS=?-Wall -Wextra -g

ASM=nasm
ASMFLAGS?=-f elf -g

LD=ld
LDFLAGS=?-m elf_i386

%.o : %.asm
	$(ASM) $(ASMFLAGS) -o $@ $<

%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.out : %.o
	%(LD) $(LDFLAGS) -o $@ $<
