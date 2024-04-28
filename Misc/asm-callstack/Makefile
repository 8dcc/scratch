
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=-no-pie

ASM=nasm
ASMFLAGS=-f elf64

OBJ_FILES=main.c.o callstack.asm.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=callstack-test.out

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c -o $@ $<

obj/%.asm.o: %.asm
	@mkdir -p $(dir $@)
	$(ASM) $(ASMFLAGS) -o $@ $<
