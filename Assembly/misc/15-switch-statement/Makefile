
CC=gcc
CFLAGS=-Wall -Wextra
ASM=nasm
ASM_FLAGS=-f elf64

BIN=switch_test.out

.PHONY: all clean

all: $(BIN)

clean:
	rm -rf ./obj/
	rm -f $(BIN)

#-------------------------------------------------------------------------------

$(BIN): obj/switch_test.c.o obj/jump_table.asm.o
	$(CC) $(CFLAGS) $^ -o $@

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(ASM) $(ASM_FLAGS) $< -o $@
