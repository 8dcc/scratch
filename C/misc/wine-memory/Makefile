
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic

OBJ_FILES=main.c.o util.c.o liblog.c.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=cod-bo3-cheat.out

#-------------------------------------------------------------------------------

.PHONY: clean all

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJS)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

obj/%.c.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<
