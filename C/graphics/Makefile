
CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra -Wpedantic -ggdb3
LDLIBS=

SDL_BINS=sdl-bresenham-line.out \
         plot.out

RAYLIB_BINS=raylib-cube.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

x-window-overlay.out: LDLIBS+=-lX11 -lXext -lXfixes -lm
$(SDL_BINS): LDLIBS+=-lm $(shell sdl2-config --cflags --libs)
$(RAYLIB_BINS): LDLIBS+=-lraylib

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)
