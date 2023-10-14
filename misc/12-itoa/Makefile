
CC=gcc
CFLAGS=-Wall -Wextra
ASM=nasm
ASM_FLAGS=-f elf64

BIN=itoa_test.out

.PHONY: all clean

all: $(BIN)

clean:
	rm -rf ./obj/
	rm -f $(BIN)

#-------------------------------------------------------------------------------

$(BIN): obj/itoa_test.c.o obj/itoa64.asm.o
	$(CC) $(CFLAGS) obj/itoa_test.c.o obj/itoa64.asm.o -o $@

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(ASM) $(ASM_FLAGS) $< -o $@
