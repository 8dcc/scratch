
CC=gcc
CFLAGS=-Wall -Wextra -ggdb3

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $<
