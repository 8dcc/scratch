
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -g
LDFLAGS=-fPIE

AS=nasm
ASFLAGS=-f elf64 -g

BIN=$(patsubst %.asm, %.out, $(wildcard ./*.asm))

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN)

#-------------------------------------------------------------------------------

%.out: obj/%.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

obj/%.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
