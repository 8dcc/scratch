
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic

#-------------------------------------------------------------------------------

.PHONY: clean all

all: main.out libtest.so

clean:
	rm -f main.out libtest.so

#-------------------------------------------------------------------------------

# Need -rdynamic so the lib can see the extern, if not, injector will give an
# error.
main.out: main.c
	$(CC) $(CFLAGS) -rdynamic -o $@ $<

# -fPIC and -shared for creating a library (shared object)
libtest.so: libtest.c
	$(CC) $(CFLAGS) -fPIC -shared -o $@ $<
