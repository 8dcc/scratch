
CC=gcc
CFLAGS=-Wall -Wextra

AS=nasm
ASFLAGS=-f elf64

OBJ_FILES=main.c.o base64.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=base64-test.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
