
CC=gcc
CFLAGS=-Wall -Wextra
ASM=nasm
ASM_FLAGS=-g -f elf32
LD=ld
LD_FLAGS=-m elf_i386

BIN=better-for-loop.out

.PHONY: all clean

all: $(BIN)

clean:
	rm -f ./obj/*.o
	rm -f $(BIN)

#-------------------------------------------------------------------------------

%.out: obj/%.asm.o
	$(CC) -m32 -no-pie $< -o $@

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(ASM) $(ASM_FLAGS) $< -o $@
