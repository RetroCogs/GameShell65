TARGET = rcPacker
DECRUNCH_TARGET = decruncher

LIBS = -lm
CC = gcc
CFLAGS = -g -Wall

HEADERS = $(wildcard *.h)
RC_SOURCES = rcPacker.c cruncher.c file.c
RC_OBJECTS = $(RC_SOURCES:.c=.o)
DECRUNCH_SOURCES = decruncher.c
DECRUNCH_OBJECTS = $(DECRUNCH_SOURCES:.c=.o)

.PHONY: default all clean

default: $(TARGET) $(DECRUNCH_TARGET)
all: default

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(DECRUNCH_TARGET) $(RC_OBJECTS) $(DECRUNCH_OBJECTS)

$(TARGET): $(RC_OBJECTS)
	$(CC) $(RC_OBJECTS) -Wall -std=c11 $(LIBS) -o $@

$(DECRUNCH_TARGET): $(DECRUNCH_OBJECTS)
	$(CC) $(DECRUNCH_OBJECTS) -Wall -std=c11 $(LIBS) -o $@

run: $(TARGET)
	./$(TARGET) ./FirstShotPAL.bin

clean:
	-rm -f *.o
	-rm -f $(TARGET) $(DECRUNCH_TARGET)
