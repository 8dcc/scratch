
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -ggdb3 -fsanitize=address
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)
