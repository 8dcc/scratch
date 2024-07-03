
# Using -no-pie because rip-relative addressing is not supported in 32-bits
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -m32 -no-pie
LDFLAGS=

AS=nasm
ASFLAGS=-f elf32

OBJ_FILES=main.c.o sieve-of-eratosthenes.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=sieve-of-eratosthenes.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) ./obj/*.o

#-------------------------------------------------------------------------------

%.out: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
