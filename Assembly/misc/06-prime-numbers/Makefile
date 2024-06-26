
CC=gcc
CFLAGS=-Wall -Wextra

AS=nasm
ASFLAGS=-f elf32

LD=ld
LDFLAGS=-m elf_i386

BIN=prime-numbers.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) ./obj/*.o

#-------------------------------------------------------------------------------

%.out: obj/%.asm.o
	$(LD) $(LDFLAGS) -o $@ $<

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
