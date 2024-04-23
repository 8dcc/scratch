
CC=gcc
CCPP=g++
CFLAGS=-Wall -Wextra -ggdb3

# Link with -lm
MATH_BINS=ppm-mandelbrot.out \
          bresenham-line.out \
          rotation-matrix.out \
          spinning-torus.out \
          generate-wav.out

SDL_BINS=sdl-bresenham-line.out

RAYLIB_BINS=raylib-cube.out

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

64bit-vars-in-32bit-pc.out: 64bit-vars-in-32bit-pc.c
	$(CC) $(CFLAGS) -m32 -o $@ $<

readline.out: readline.c
	$(CC) $(CFLAGS) -o $@ $< -lreadline

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

$(MATH_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm

$(SDL_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm `sdl2-config --cflags --libs`

$(RAYLIB_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lraylib

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CCPP) $(CFLAGS) -o $@ $<
