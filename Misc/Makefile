CC=gcc
CFLAGS=-Wall

all:
	@echo "No arguments. Try running:"
	@echo "  make array-slider.out"
	@echo "  make encode.out"
	@echo "  make fizzbuzz.out"
	@echo "  make getchar-test.out"
	@echo "  make length-macro.out"
	@echo "  make ncurses.out"
	@echo "  make stack-test.out"
	@echo "  make struct-pointers.out"

array-slider.out: array-slider.c
	$(CC) $(CFLAGS) -o $@ $<

encode.out: encode.c
	$(CC) $(CFLAGS) -o $@ $<

fizzbuzz.out: fizzbuzz.c
	$(CC) $(CFLAGS) -o $@ $<

getchar-test.out: getchar-test.c
	$(CC) $(CFLAGS) -o $@ $<

length-macro.out: length-macro.c
	$(CC) $(CFLAGS) -o $@ $<

ncurses.out: ncurses.c
	$(CC) $(CFLAGS) -o $@ $< -lncurses

stack-test.out: stack-test.c
	$(CC) $(CFLAGS) -o $@ $<

struct-pointers.out: struct-pointers.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm *.out
