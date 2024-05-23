
CC=gcc
CFLAGS=-Wall -Wextra -ggdb3

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

ppm-mandelbrot.out: ppm-mandelbrot.c
	$(CC) $(CFLAGS) -o $@ $< -lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<
