
CC=gcc
CXX=g++
CFLAGS=-Wall -Wextra -ggdb3

# Link with -lm
MATH_BINS=bresenham-line.out \
          rotation-matrix.out \
          spinning-torus.out \
          generate-wav.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

$(MATH_BINS): %.out: %.c
	$(CC) $(CFLAGS) -o $@ $< -lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<
