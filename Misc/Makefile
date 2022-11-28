
CC=gcc
CFLAGS=-Wall

all:
	@echo "No arguments. Try running:"
	@echo "  make array-slider.out"
	@echo "  make encode.out"
	@echo "  make fizzbuzz.out"
	@echo "  make function-pointers.out"
	@echo "  make getchar-test.out"
	@echo "  make htonl-test.out"
	@echo "  make length-macro.out"
	@echo "  make ncurses.out"
	@echo "  make stack-test.out"
	@echo "  make static-recursion.c"
	@echo "  make struct-pointers.out"

array-slider.out: array-slider.c
	$(CC) $(CFLAGS) -o $@ $<

encode.out: encode.c
	$(CC) $(CFLAGS) -o $@ $<

fizzbuzz.out: fizzbuzz.c
	$(CC) $(CFLAGS) -o $@ $<

function-pointers.out: function-pointers.c
	$(CC) $(CFLAGS) -o $@ $<

getchar-test.out: getchar-test.c
	$(CC) $(CFLAGS) -o $@ $<

htonl-test.out: htonl-test.c
	$(CC) $(CFLAGS) -o $@ -g $<

length-macro.out: length-macro.c
	$(CC) $(CFLAGS) -o $@ $<

ncurses.out: ncurses.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

stack-test.out: stack-test.c
	$(CC) $(CFLAGS) -o $@ $<

static-recursion.out: static-recursion.c
	$(CC) $(CFLAGS) -o $@ $<

struct-pointers.out: struct-pointers.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm *.out
