
CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra -Wpedantic -ggdb3
LDLIBS=

OBJ=obj/main.c.o obj/pool.c.o
BIN=pool-allocator-test.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJ)

#-------------------------------------------------------------------------------

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS)

obj/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<
