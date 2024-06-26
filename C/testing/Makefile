
CC=gcc
CXX=g++
CFLAGS=-Wall -Wextra -ggdb3
LDFLAGS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

readline.out: readline.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) -lreadline

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) -lncurses

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $< $(LDFLAGS)
