
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

# Link with math library
shannon-entropy.out: LDLIBS+=-lm

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $< $(LDLIBS)
