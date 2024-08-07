
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

ppm-mandelbrot.out:  LDLIBS+=-lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)
