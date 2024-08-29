
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -g -no-pie

AS=nasm
ASFLAGS=-f elf64 -g

BIN=xchg-rax-rax.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN)

#-------------------------------------------------------------------------------

%.out: obj/%.o
	$(CC) $(CFLAGS) -o $@ $^

obj/%.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
