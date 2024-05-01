# NOTE: I rather use a Makefile than the Ada .gpr files, at least for now.

CC=gcc
CFLAGS=-Wall -Wextra -Wpedantic -ggdb3

# Not the best way of doing it
BINS=$(patsubst %.adb, %.out, $(wildcard *.adb))
OBJS=$(addprefix obj/, $(patsubst %.out, %.o, $(BINS)))
ALIS=$(addprefix obj/, $(patsubst %.out, %.ali, $(BINS)))

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.adb, %.out, $(wildcard ./*.adb))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

# Compile with GCC, uses the same process as `gnatmake'
$(OBJS): obj/%.o : %.adb
	@mkdir -p $(dir $@)
	$(CC) -c -o $@ $<

# Compiler generates .ali files along with .o files
$(ALIS): obj/%.ali : obj/%.o

# TODO: What is binding for, exactly?
$(BINS): %.out : obj/%.ali
	gnatbind -x $<
	gnatlink -o $@ $<
