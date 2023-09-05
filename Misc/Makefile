
CC=gcc
CCPP=g++
CFLAGS=-Wall -Wextra -g

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

ppm-mandelbrot.out: ppm-mandelbrot.c
	$(CC) $(CFLAGS) -o $@ $< -lm

bresenham-line.out: bresenham-line.c
	$(CC) $(CFLAGS) -o $@ $< -lm

sdl-bresenham-line.out: sdl-bresenham-line.c
	$(CC) $(CFLAGS) -o $@ $< -lm `sdl2-config --cflags --libs`

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CCPP) $(CFLAGS) -o $@ $<
