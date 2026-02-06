#include "cruncher.h"
#include <stdio.h>

//#define DEBUG

#define max_offs       0x129f // 12bit offset limit. $129f
#define max_offs_short 0x054f // 10bit offset limit. $054f

uint offsTab[4] = {5,2,2,3};
uint offsTabShort[4] = {4,2,2,2};

byte *ibuf;
byte obuf[memSize];
uint ibufSize;
int get; //points to in[]
uint put; //points to out[]

bool copyFlag;
byte curByte;
byte curCnt;
uint plainLen;

bool errorFlag;

#define decrLen 0x11a
byte decr[decrLen] = {
  0x0B, 0x08, 0x00, 0x00, 0x9E, 0x32, 0x30, 0x36, 0x31, 0x00, 0x00, 0x00, 0x78, 0xA9, 0x34, 0x85
, 0x01, 0xA2, 0x3B, 0xBD, 0x1F, 0x08, 0x95, 0x06, 0xCA, 0x10, 0xF8, 0x4C, 0x08, 0x00, 0x00, 0x00
, 0xA0, 0xFF, 0xB9, 0x00, 0x00, 0x99, 0x00, 0xFF, 0x88, 0xD0, 0xF7, 0xB1, 0x0B, 0x91, 0x0E, 0xC6
, 0x0F, 0xC6, 0x0C, 0xA5, 0x0C, 0xC9, 0x08, 0xB0, 0xE7, 0xB9, 0x5B, 0x08, 0x99, 0x34, 0x03, 0xC8
, 0xD0, 0xF7, 0xA0, 0x02, 0xB1, 0x06, 0x99, 0x03, 0x00, 0x88, 0x10, 0xF8, 0x18, 0xA9, 0x03, 0x65
, 0x06, 0x85, 0x06, 0x90, 0x02, 0xE6, 0x07, 0x4C, 0x34, 0x03, 0x20, 0xB8, 0x03, 0x08, 0xA9, 0x01
, 0x20, 0xB8, 0x03, 0x90, 0x06, 0x20, 0xB8, 0x03, 0x2A, 0x10, 0xF5, 0x28, 0xB0, 0x18, 0x85, 0x02
, 0xA0, 0x00, 0xB1, 0x06, 0x91, 0x04, 0xC8, 0xC4, 0x02, 0xD0, 0xF7, 0xA2, 0x02, 0x20, 0xDD, 0x03
, 0xC8, 0xF0, 0xD7, 0x38, 0xB0, 0xD7, 0x69, 0x00, 0xF0, 0x4C, 0x85, 0x02, 0xC9, 0x03, 0xA9, 0x00
, 0x85, 0x08, 0x85, 0x09, 0x2A, 0x20, 0xB8, 0x03, 0x2A, 0x20, 0xB8, 0x03, 0x2A, 0xAA, 0xBC, 0xEC
, 0x03, 0x20, 0xB8, 0x03, 0x26, 0x08, 0x26, 0x09, 0x88, 0xD0, 0xF6, 0x8A, 0xCA, 0x29, 0x03, 0xF0
, 0x08, 0xE6, 0x08, 0xD0, 0xE9, 0xE6, 0x09, 0xD0, 0xE5, 0x38, 0xA5, 0x04, 0xE5, 0x08, 0x85, 0x08
, 0xA5, 0x05, 0xE5, 0x09, 0x85, 0x09, 0xB1, 0x08, 0x91, 0x04, 0xC8, 0xC4, 0x02, 0xD0, 0xF7, 0xA2
, 0x00, 0x20, 0xDD, 0x03, 0x30, 0x84, 0xA9, 0x37, 0x85, 0x01, 0x58, 0x4C, 0x00, 0x00, 0x06, 0x03
, 0xD0, 0x20, 0x48, 0x98, 0x48, 0xA0, 0x00, 0xB1, 0x06, 0xE6, 0x06, 0xD0, 0x02, 0xE6, 0x07, 0x38
, 0x2A, 0x85, 0x03, 0xA9, 0x05, 0xE6, 0x01, 0x8D, 0x20, 0xD0, 0x8C, 0x20, 0xD0, 0xC6, 0x01, 0x68
, 0xA8, 0x68, 0x60, 0x18, 0x98, 0x75, 0x04, 0x95, 0x04, 0x90, 0x02, 0xF6, 0x05, 0xCA, 0xCA, 0x10
, 0xF2, 0x60, 0x04, 0x02, 0x02, 0x02, 0x05, 0x02, 0x02, 0x03 };

void init(File *aSource)
{
  get = ibufSize - 1;
  put = memSize - 1;
  curByte = 0;
  curCnt = 8;

  plainLen = 0;
}

void out(byte b)
{
#ifdef DEBUG
  printf("%i", b);
#endif

  curByte >>= 1;
  curByte |= (b << 7);
  if((--curCnt) == 0) {
#ifdef DEBUG
    printf("|");
#endif
    obuf[put] = curByte;
    if(put == 0) {
      printf("Error (C-1): Packed file too large.\n");
      put = memSize - 1; // Avoid more damage..
      errorFlag = true;
    }
    --put;
    curCnt = 8;
    curByte = 0;
  }
}

void outLen(byte b)
{
#ifdef DEBUG
  printf(";");
#endif

  if(b < 0x80)
    out(0);

  while(b > 1) {
    out(b & 1);
    out(1);
    b >>= 1;
  }
}

void outCopyFlag()
{
  if(copyFlag == 1) {
    out(1);
    copyFlag = 0;
  }
}

bool scan(uint *theMatchLen, uint *theMatchOffset)
{
  uint scn;
  uint matchLen = 0;
  uint matchOffset = 0;

  if(get < 2) {
    return false;
  }

  scn = get - 1;

  register byte first = ibuf[get];
  register byte second = ibuf[get - 1];

  while(((get - scn) <= max_offs) &&
        (scn > 0)){
    if((ibuf[scn] == first) &&
       (ibuf[scn - 1] == second)) {
      uint len = 2;
      while((len < 255) &&
            (scn >= len) &&
            (ibuf[scn - len] == ibuf[get - len])) {
        ++len;
      };

      if(len > matchLen) {
        matchLen = len;
        matchOffset = get - scn;
      }
    }
    --scn;
  };

  if((matchLen > 2) ||
     ((matchLen == 2) && (matchOffset <= max_offs_short))) {
    *theMatchLen = matchLen;
    *theMatchOffset = matchOffset;
    return true;
  }
  else {
    return false;
  }
}

void copy(uint matchLen, uint matchOffset)
{
  uint i = 0;

  copyFlag = 1;

#ifdef DEBUG
  printf("C(%i, %i) ", matchLen, matchOffset);
#endif

  // Put copy offset.
  while(i < 4) {
    uint b;
    if(matchLen == 2) {
      b = offsTabShort[i];
    }
    else {
      b = offsTab[i];
    }
    while(b > 0) {
      out(matchOffset & 1);
      matchOffset >>= 1;
      --b;
    };

    if(matchOffset == 0)
      break;

    --matchOffset;
    ++i;
  };

  // Put copy offset size.
  out(i & 1);
  out((i >> 1) & 1);

  // Put copy length.
  outLen(matchLen - 1);

  get -= matchLen;
}

void flush()
{
  // Exit if there is nothing to flush.
  if(plainLen == 0) {
    outCopyFlag();
    return;
  }

#ifdef DEBUG
  printf("P(%i) ", plainLen);
#endif

  // Put extra copy-bit if necessary.
  if((plainLen % 255) == 0) {
    outCopyFlag();
  }

  // Put plain data.
  while(plainLen > 0) {
    uint i;
    uint len = plainLen % 255;

    if(put < len) {
      printf("Error (C-2): Packed file too large.\n");
      put = memSize - 1; // Avoid more damage..
      errorFlag = true;
    }

    // Copy the data.
    for(i = 0; i < len; ++i) {
      obuf[put - i] = ibuf[get + plainLen - i];
    }

    plainLen -= len;
    put -= len;

    // Put plain length.
    outLen(len);

    // Put plain-bit.
    out(0);
  };

  plainLen = 0;
}


bool crunch(File *aSource, File *aTarget, uint startAdress)
{
  uint i;
  uint theMatchLen, theMatchOffset; 
  uint packLen;
  byte *target;
  bool attachDecr;

  errorFlag = false;
  attachDecr = (startAdress == 0 ? false : true);

  ibufSize = aSource->size - 2;
  ibuf = (byte *)malloc(ibufSize);
  for(i = 0; i < ibufSize; ++i) {
    ibuf[i] = aSource->data[i + 2];
  }

  init(aSource);

  outLen(0xff); // Put end of file.
  copyFlag = 1;

#ifdef DEBUG
  printf(".");
#endif

  while(get >= 0) {
    if(scan(&theMatchLen, &theMatchOffset)) {
      flush();
      copy(theMatchLen, theMatchOffset);
    }
    else {
      ++plainLen;
      --get;
    }
  };
  flush();

  if(errorFlag == true) {
    return false;
  }

  //Copy obuf into aTarget!!
  packLen = memSize - put - 1;

#ifdef DEBUG
  printf("\nsize = %i\n", packLen);
#endif

  aTarget->size = packLen + 5;
  if(attachDecr) {
    aTarget->size += decrLen;
  }
  aTarget->data = (byte *)malloc(aTarget->size);
  if(aTarget->data == NULL) {
    printf("Error (C-3): Out of memory.\n");
    return false;
  }

  if(attachDecr)
    target = aTarget->data + decrLen + 2;
  else 
    target = aTarget->data + 2;


  target[0] = (curByte | (1 << (curCnt - 1)));
  target[1] = aSource->data[0];
  target[2] = aSource->data[1];

  for(i = 0; i < packLen; ++i) {
    target[i + 3] = obuf[put + i + 1];
  }

  if(attachDecr) {
    uint packStart = 0xfffd - packLen;
    uint trnsStart = 0x081e + packLen;
    decr[0x1e] = packStart & 0xff;
    decr[0x1f] = packStart >> 8;
    decr[0x23] = trnsStart & 0xff;
    decr[0x24] = trnsStart >> 8;
    decr[0xdc] = startAdress & 0xff;
    decr[0xdd] = startAdress >> 8;

    target = aTarget->data + 2;
    for(i = 0; i < decrLen; ++i) {
      target[i] = decr[i];
    }

    aTarget->data[0] = 0x01;
    aTarget->data[1] = 0x08;
  } else {
    uint packStart = 0xfffa - (packLen + 3);
    aTarget->data[0] = packStart & 0xff;
    aTarget->data[1] = packStart >> 8;
  }

  free(ibuf);

  return true;
}
