#ifndef _file_h_
#define _file_h_

#include "bb.h"

#include <stdio.h>
#include <sys/stat.h>

typedef struct {
  size_t size;
  byte *data;
} Buffer;

typedef struct {
  char *name;
  Buffer buffer;
} File;

void freeFile(File *aFile);
bool readFile(File *aFile, const char *fileName);
bool writeFile(const Buffer *aBuffer, const char *fileName);

#endif // _file_h_
