
CC=gcc
CFLAGS=-Wall -Wextra -ggdb3
LDFLAGS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

ppm-mandelbrot.out: ppm-mandelbrot.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) -lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)
