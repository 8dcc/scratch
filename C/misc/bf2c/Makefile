
CC=gcc
CFLAGS=-Wall -Wextra

#-------------------------------------------------------------------------------

.PHONY: all clean

all: output.out

clean:
	rm -f output.out output.c bf2c.out

#-------------------------------------------------------------------------------

output.out: output.c
	$(CC) -o $@ $^

output.c: bf2c.out input.bf
	./bf2c.out < input.bf > output.c

bf2c.out: bf2c.c
	$(CC) $(CFLAGS) -o $@ $^
