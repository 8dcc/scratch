
CC=gcc
CXX=g++
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

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

$(MATH_BINS): LDLIBS+=-lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $< $(LDLIBS)
