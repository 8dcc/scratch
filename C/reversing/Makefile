
CC=gcc
CXX=g++
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

64bit-vars-in-32bit-pc.out: CFLAGS+=-m32

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)
