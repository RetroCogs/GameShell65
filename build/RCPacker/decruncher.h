#ifndef _decruncher_h_
#define _decruncher_h_

#include "file.h"

int decrunchToB3(const File *packedFile, const char *inputName, File *outB3File);
int validateOriginalVsB3(const File *originalFile, const File *b3File);

#endif // _decruncher_h_
