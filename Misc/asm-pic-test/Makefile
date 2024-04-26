CC=gcc
CFLAGS=-Wall -Wextra -Ofast
LDFLAGS=-shared -no-pie

AS=nasm
ASFLAGS=-f elf64

.PHONY: all clean

all: asm-pic-test.out

clean:
	rm -f asm-pic-test.out main.o test.o

asm-pic-test.out: main.o test.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

main.o: main.c
	$(CC) $(CFLAGS) -c -o $@ $<

test.o: test.asm
	$(AS) $(ASFLAGS) -o $@ $<
