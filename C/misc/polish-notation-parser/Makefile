
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic
LDLIBS=-lreadline

SRC=main.c parser.c calc.c
OBJ=$(addprefix obj/, $(addsuffix .o, $(SRC)))

BIN=polish-notation-parser.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(BIN) $(OBJ)

#-------------------------------------------------------------------------------

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS)

obj/%.c.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<
