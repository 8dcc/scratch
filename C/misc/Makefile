
CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra -ggdb3
LDLIBS=

# Link with -lm
MATH_BINS=bresenham-line.out \
          rotation-matrix.out \
          spinning-torus.out \
          generate-wav.out \
          baseN.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

$(MATH_BINS): LDLIBS+=-lm
websocket-handshake.out: LDLIBS+=-lcrypto -lb64

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)
