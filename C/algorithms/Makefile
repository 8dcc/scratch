
CC=gcc
CFLAGS=-Wall -Wextra -Wpedantic -ggdb3

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<
