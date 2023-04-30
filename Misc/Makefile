
CC=gcc
CFLAGS=-Wall -Wextra -g

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<
