
CC=gcc
CXX=g++
CFLAGS=-std=gnu99 -Wall -Wextra -Wpedantic -ggdb3

SDL_BINS=sdl-bresenham-line.out \
         plot.out

RAYLIB_BINS=raylib-cube.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

x-window-overlay.out: x-window-overlay.c
	$(CC) $(CFLAGS) -o $@ $< -lX11 -lXext -lXfixes -lm

$(SDL_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm $(shell sdl2-config --cflags --libs)

$(RAYLIB_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lraylib

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<
