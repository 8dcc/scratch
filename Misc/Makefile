CC=gcc
CFLAGS=-Wall

all:
	@echo "No arguments. Try running:"
	@echo "  make array_slider.out"
	@echo "  make encode.out"
	@echo "  make fizzbuzz.out"
	@echo "  make getchar_test.out"
	@echo "  make ncurses.out"
	@echo "  make stack-test.out"
	@echo "  make struct-pointers.out"

array_slider.out: array_slider.c
	$(CC) $(CFLAGS) -o $@ $<

encode.out: encode.c
	$(CC) $(CFLAGS) -o $@ $<

fizzbuzz.out: fizzbuzz.c
	$(CC) $(CFLAGS) -o $@ $<

getchar_test.out: getchar_test.c
	$(CC) $(CFLAGS) -o $@ $<

ncurses.out: ncurses.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

stack-test.out: stack-test.c
	$(CC) $(CFLAGS) -o $@ $<

struct-pointers.out: struct-pointers.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm *.out
