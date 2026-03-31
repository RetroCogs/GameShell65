TARGET = rcPacker

LIBS = -lm
CXX = g++
CXXFLAGS = -g -O3 -Wall -std=c++17 -x c++

HEADERS = $(wildcard *.h)
RC_SOURCES = rcPacker.c cruncher.c file.c decruncher.c validation.c
RC_OBJECTS = $(RC_SOURCES:.c=.o)

.PHONY: default all clean

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(RC_OBJECTS)

$(TARGET): $(RC_OBJECTS)
	$(CXX) $(RC_OBJECTS) $(LIBS) -o $@

run: $(TARGET)
	./$(TARGET) ./FirstShotPAL.bin

clean:
	-rm -f *.o
	-rm -f $(TARGET)
