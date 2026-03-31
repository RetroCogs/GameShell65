#ifndef _bb_h_
#define _bb_h_

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL ((void*)0)
#endif
#endif

#ifndef byte
typedef unsigned char byte;
#endif
#ifndef uint
typedef unsigned int uint;
#endif

#ifndef __cplusplus
typedef enum { false = 0, true = 1 } bool;
#endif

#define memSize 65536

#endif // _bb_h_
