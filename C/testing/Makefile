
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic

CXX=g++
CXXFLAGS=-std=gnu++20 -Wall -Wextra -Wpedantic

LDFLAGS=

#-------------------------------------------------------------------------------

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
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

%.out: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)
