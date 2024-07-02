
CC=gcc
CXX=g++
CFLAGS=-std=gnu99 -Wall -Wextra -Wpedantic -ggdb3
LDFLAGS=

SDL_BINS=sdl-bresenham-line.out \
         plot.out

RAYLIB_BINS=raylib-cube.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

x-window-overlay.out: LDFLAGS+=-lX11 -lXext -lXfixes -lm
$(SDL_BINS): LDFLAGS+=-lm $(shell sdl2-config --cflags --libs)
$(RAYLIB_BINS): LDFLAGS+=-lraylib

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $< $(LDFLAGS)
