TARGET = rcPacker

LIBS = -lm
CC = gcc
CFLAGS = -g -Wall

HEADERS = $(wildcard *.h)
RC_SOURCES = rcPacker.c cruncher.c file.c decruncher.c
RC_OBJECTS = $(RC_SOURCES:.c=.o)

.PHONY: default all clean

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(RC_OBJECTS)

$(TARGET): $(RC_OBJECTS)
	$(CC) $(RC_OBJECTS) -Wall -std=c11 $(LIBS) -o $@

run: $(TARGET)
	./$(TARGET) ./FirstShotPAL.bin

clean:
	-rm -f *.o
	-rm -f $(TARGET)
