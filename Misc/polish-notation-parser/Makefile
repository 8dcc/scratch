
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=-lreadline

OBJ_FILES=main.c.o parser.c.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=polish-notation-parser.out

.PHONY: clean all

# -------------------------------------------

all: $(BIN)

clean:
	rm -f $(OBJS)
	rm -f $(BIN)

# -------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS)

obj/%.c.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c -o $@ $<
