
CC=gcc
CCPP=g++
CFLAGS=-Wall -Wextra -g

# Link with -lm
MATH_BINS=ppm-mandelbrot.out bresenham-line.out rotation-matrix.out spinning-torus.out

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

sdl-bresenham-line.out: sdl-bresenham-line.c
	$(CC) $(CFLAGS) -o $@ $< -lm `sdl2-config --cflags --libs`

64bit-vars-in-32bit-pc.out: 64bit-vars-in-32bit-pc.c
	$(CC) $(CFLAGS) -m32 -o $@ $<

readline.out: readline.c
	$(CC) $(CFLAGS) -o $@ $< -lreadline

ncurses-%.out: ncurses-%.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

$(MATH_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CCPP) $(CFLAGS) -o $@ $<
