#ifndef _decruncher_h_
#define _decruncher_h_

#include "file.h"

int decrunchToMemory(const File *packedFile, const char *inputName, File *outB3File);

#endif // _decruncher_h_
