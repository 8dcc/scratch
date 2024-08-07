
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=-fPIE

AS=nasm
ASFLAGS=-f elf64

OBJ_FILES=main.c.o callstack.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=callstack-test.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
