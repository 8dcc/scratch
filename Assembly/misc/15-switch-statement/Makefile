
CC=gcc
CFLAGS=-Wall -Wextra

AS=nasm
ASFLAGS=-f elf64

OBJ_FILES=switch_test.c.o jump_table.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=switch_test.out

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
