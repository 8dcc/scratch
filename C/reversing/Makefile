
CC=gcc
CXX=g++
CFLAGS=-Wall -Wextra -ggdb3
LDFLAGS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.c, %.out, $(wildcard ./*.c)) $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

64bit-vars-in-32bit-pc.out: 64bit-vars-in-32bit-pc.c
	$(CC) $(CFLAGS) -m32 -o $@ $^ $(LDFLAGS)

%.out: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

%.out: %.cpp
	$(CXX) $(CFLAGS) -o $@ $< $(LDFLAGS)
