
CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra -Wpedantic -ggdb3
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: pool-allocator-test.out

clean:
	rm -f pool-allocator-test.out obj/main.c.o obj/pool.c.o

#-------------------------------------------------------------------------------

pool-allocator-test.out: obj/main.c.o obj/pool.c.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS)

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<
