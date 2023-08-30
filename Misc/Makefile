
CC=gcc
CCPP=g++
CFLAGS=-Wall -Wextra -g

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

bresenham-line.out: bresenham-line.c
	$(CC) $(CFLAGS) -o $@ $< -lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CCPP) $(CFLAGS) -o $@ $<
