
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic

SRC=util.c liblog.c
OBJ=$(addprefix obj/, $(addsuffix .o, $(SRC)))

#-------------------------------------------------------------------------------

.PHONY: all clean

all: cod-bo3-cheat1.out cod-bo3-cheat2.out

clean:
	rm -f $(OBJ)
	rm -f cod-bo3-cheat1.out cod-bo3-cheat2.out

#-------------------------------------------------------------------------------

cod-bo3-cheat%.out: obj/main%.c.o $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

obj/%.c.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<
