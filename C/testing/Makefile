
CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra -Wpedantic
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

readline.out: LDLIBS+=-lreadline
ncurses-%.out: LDLIBS+=-lncurses

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)
