
CC=gcc
CXX=g++
CFLAGS=-Wall -Wextra -ggdb3

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

64bit-vars-in-32bit-pc.out: 64bit-vars-in-32bit-pc.c
	$(CC) $(CFLAGS) -m32 -o $@ $<

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<
