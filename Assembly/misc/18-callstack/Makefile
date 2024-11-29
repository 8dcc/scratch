
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=-fPIE

AS=nasm
ASFLAGS=-f elf64

SRC=main.c callstack.asm
OBJ=$(addprefix obj/, $(addsuffix .o, $(SRC)))

BIN=callstack-test.out

# ------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJ)

# ------------------------------------------------------------------------------

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

obj/main.c.o: main.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<

obj/callstack.asm.o: callstack.asm
	@mkdir -p $(dir $@)
	$(AS) $(ASFLAGS) -o $@ $<
