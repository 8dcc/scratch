
CC=gcc
CFLAGS=-Wall -Wextra -g

OBJS=itoa64.o itoa_test.o
BIN=itoa_test.out

.PHONY: all

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o : %.asm
	nasm -f elf64 $< -o $@

