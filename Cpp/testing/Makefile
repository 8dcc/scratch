
CXX=g++
CXXFLAGS=-std=gnu++20 -Wall -Wextra -Wpedantic
LDLIBS=

#-------------------------------------------------------------------------------

.PHONY: all clean

all: $(patsubst %.cpp, %.out, $(wildcard ./*.cpp))

clean:
	rm -f *.out

#-------------------------------------------------------------------------------

%.out: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDLIBS)
