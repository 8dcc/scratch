
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic
LDFLAGS=

#-------------------------------------------------------------------------------

.PHONY: clean all

all: encrypt.out decrypt.out

clean:
	rm -f ./*.out

#-------------------------------------------------------------------------------

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)
