
CC=gcc
CXX=g++
CFLAGS=-Wall -Wextra -ggdb3

# Link with -lm
 MATH_BINS=bresenham-line.out \
           rotation-matrix.out \
           spinning-torus.out \
           generate-wav.out

SDL_BINS=sdl-bresenham-line.out

RAYLIB_BINS=raylib-cube.out

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

x-window-overlay.out: x-window-overlay.c
	$(CC) $(CFLAGS) -o $@ $< -lX11 -lXext -lXfixes -lm

$(MATH_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm

$(SDL_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm $(shell sdl2-config --cflags --libs)

$(RAYLIB_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lraylib

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<
