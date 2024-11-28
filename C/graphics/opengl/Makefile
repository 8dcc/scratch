
CC=gcc
CFLAGS=-std=c99 -Wall -Wextra
LDLIBS=-lglfw

SRC=main.c deps/glad.c
OBJ=$(addprefix obj/, $(addsuffix .o, $(SRC)))

BIN=opengl-test.out

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(BIN)

clean:
	rm -f $(OBJ)
	rm -f $(BIN)

#-------------------------------------------------------------------------------

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS)

obj/%.c.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -o $@ -c $<
