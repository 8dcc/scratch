
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=

ASM=nasm
ASMFLAGS=-f elf64

OBJS=memcpy-test.o memcpy-asm.o
BIN=memcpy-test.out

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

memcpy-test.o: memcpy-test.c
	$(CC) $(CFLAGS) -c -o $@ $<

# NOTE: Can't name the file `memcpy.asm' because of make's implicit rules. I
# don't really understand it, but whatever.
memcpy-asm.o: memcpy-asm.asm
	$(ASM) $(ASMFLAGS) -o $@ $<
