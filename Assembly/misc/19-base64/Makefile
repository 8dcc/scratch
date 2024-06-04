
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=

ASM=nasm
ASMFLAGS=-f elf64

OBJS=test.o base64.o
BIN=base64-test.out

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.asm
	$(ASM) $(ASMFLAGS) -o $@ $<
