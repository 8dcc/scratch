
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic

CXX=g++
CXXFLAGS=-std=gnu++20 -Wall -Wextra -Wpedantic

LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

readline.out: LDLIBS+=-lreadline
ncurses-%.out: LDLIBS+=-lncurses

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)

%.out: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDLIBS)
