#ifndef _BB_h_
#define _BB_h_

#include <stdlib.h>
#include <string.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifndef byte
#define byte unsigned char
#endif
#ifndef uint
#define uint unsigned int
#endif

typedef enum { false = 0, true = 1 } bool;

#define memSize 65536

#endif // _BB_h_
