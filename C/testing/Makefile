
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

readline.out: LDFLAGS+=-lreadline
ncurses-%.out: LDFLAGS+=-lncurses

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

%.out: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)
