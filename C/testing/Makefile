
CC=gcc
CXX=g++
CFLAGS=-Wall -Wextra -ggdb3

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

readline.out: readline.c
	$(CC) $(CFLAGS) -o $@ $< -lreadline

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<
