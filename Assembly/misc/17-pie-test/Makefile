
CC=gcc
CFLAGS=-Wall -Wextra -Ofast
LDFLAGS=-shared -no-pie

AS=nasm
ASFLAGS=-f elf64

OBJ_FILES=main.c.o test.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=asm-pic-test.out

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
