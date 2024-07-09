
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -m32
LDFLAGS=

AS=nasm
ASFLAGS=-f elf32

OBJ_FILES=main.c.o abs.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=abs-test.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
