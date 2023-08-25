CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=

.PHONY: clean all

all: encrypt.out decrypt.out

clean:
	rm -f ./*.out

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)
