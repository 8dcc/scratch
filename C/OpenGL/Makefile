
CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=-lglfw

OBJ_FILES=main.c.o glad.c.o
OBJS=$(addprefix obj/, $(OBJ_FILES))

BIN=opengl-test.out

#-------------------------------------------------------------------------------

.PHONY: clean all install

all: $(BIN)

clean:
	rm -f $(OBJS)
	rm -f $(BIN)

#-------------------------------------------------------------------------------

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS)

obj/%.c.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c -o $@ $<
