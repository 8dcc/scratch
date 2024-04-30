
CC=gcc
CFLAGS=-Wall -Wextra
ASM=nasm
ASM_FLAGS=-f elf32
LD=ld
LD_FLAGS=-m elf_i386

BIN=count-characters.out

.PHONY: all clean

all: $(BIN)

clean:
	rm -rf ./obj/*.o
	rm -f $(BIN)

#-------------------------------------------------------------------------------

%.out: obj/%.asm.o
	$(LD) $(LD_FLAGS) $< -o $@

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(ASM) $(ASM_FLAGS) $< -o $@
