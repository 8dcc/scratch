
CC=gcc
CFLAGS=-Wall -Wextra -m32 -no-pie

AS=nasm
ASFLAGS=-g -f elf32

BIN=arithmetic-branching.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) ./obj/*.o

#-------------------------------------------------------------------------------

%.out: obj/%.asm.o
	$(CC) $(CFLAGS) -o $@ $<

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
