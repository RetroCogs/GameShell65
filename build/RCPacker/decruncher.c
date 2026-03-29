#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "decruncher.h"

typedef struct {
  const byte *src;
  size_t srcSize;
  size_t srcPos;
  byte curBitByte;
  uint bitsLeft;
} BitStream;

static bool writeRawFile(const char *fileName, const byte *data, size_t size)
{
  FILE *fp = fopen(fileName, "wb");
  if (fp == NULL) {
    return false;
  }

  if (fwrite(data, 1, size, fp) != size) {
    fclose(fp);
    return false;
  }

  fclose(fp);
  return true;
}

static char *makeSuffixedName(const char *fileName, const char *suffix)
{
  size_t baseLen = strlen(fileName);
  size_t suffixLen = strlen(suffix);
  char *out = (char *)malloc(baseLen + suffixLen + 1);

  if (out == NULL) {
    return NULL;
  }

  memcpy(out, fileName, baseLen);
  memcpy(out + baseLen, suffix, suffixLen);
  out[baseLen + suffixLen] = '\0';
  return out;
}

static bool bsReadByte(BitStream *bs, byte *out)
{
  if (bs->srcPos >= bs->srcSize) {
    return false;
  }

  *out = bs->src[bs->srcPos++];
  return true;
}

static bool bsReadBit(BitStream *bs, uint *outBit)
{
  if (bs->bitsLeft == 0) {
    if (!bsReadByte(bs, &bs->curBitByte)) {
      return false;
    }
    bs->bitsLeft = 8;
  }

  *outBit = (bs->curBitByte & 0x80) ? 1u : 0u;
  bs->curBitByte <<= 1;
  bs->bitsLeft--;
  return true;
}

static bool readLength(BitStream *bs, uint *outLen)
{
  uint len = 1;

  while (1) {
    uint b;

    if (!bsReadBit(bs, &b)) {
      return false;
    }

    if (b == 0) {
      *outLen = len;
      return true;
    }

    if (!bsReadBit(bs, &b)) {
      return false;
    }

    len = (len << 1) | b;

    if (len >= 128) {
      *outLen = len;
      return true;
    }
  }
}

static uint bitsForOffsetGroup(uint group, uint lenField)
{
  static const uint shortBits[4] = {3, 6, 8, 10};
  static const uint longBits[4] = {4, 7, 10, 13};

  if (group > 3) {
    return 0;
  }

  if (lenField == 1) {
    return shortBits[group];
  }

  return longBits[group];
}

static bool readOffset(BitStream *bs, uint lenField, uint *outOffset)
{
  uint b0;
  uint b1;
  uint group;
  uint n;
  uint value = 0;
  uint mask;

  if (!bsReadBit(bs, &b0) || !bsReadBit(bs, &b1)) {
    return false;
  }

  group = (b0 << 1) | b1;
  n = bitsForOffsetGroup(group, lenField);
  if (n == 0) {
    return false;
  }

  if (n >= 8) {
    byte low;

    mask = (1u << n);
    while (mask > 0x100) {
      uint bit;
      mask >>= 1;
      if (!bsReadBit(bs, &bit)) {
        return false;
      }
      if (bit) {
        value |= mask;
      }
    }

    if (!bsReadByte(bs, &low)) {
      return false;
    }
    value |= (uint)(low ^ 255u);
  } else {
    mask = (1u << n);
    while (mask > 1) {
      uint bit;
      mask >>= 1;
      if (!bsReadBit(bs, &bit)) {
        return false;
      }
      if (bit == 0) {
        value |= mask;
      }
    }
  }

  *outOffset = value;
  return true;
}

static bool appendByte(byte **out, size_t *outSize, size_t *cap, byte v)
{
  if (*outSize >= *cap) {
    byte *newData;
    size_t newCap = (*cap == 0) ? 1024 : (*cap * 2);
    newData = (byte *)realloc(*out, newCap);
    if (newData == NULL) {
      return false;
    }
    *out = newData;
    *cap = newCap;
  }

  (*out)[*outSize] = v;
  (*outSize)++;
  return true;
}

static bool decrunchPayload(const byte *packed, size_t packedSize, byte **outPayload, size_t *outPayloadSize)
{
  BitStream bs;
  byte *payload = NULL;
  size_t payloadSize = 0;
  size_t payloadCap = 0;
  bool needCopyBit = true;

  bs.src = packed;
  bs.srcSize = packedSize;
  bs.srcPos = 0;
  bs.curBitByte = 0;
  bs.bitsLeft = 0;

  while (1) {
    uint tokenIsMatch = 1;

    if (needCopyBit) {
      if (!bsReadBit(&bs, &tokenIsMatch)) {
        free(payload);
        return false;
      }
    }

    if (tokenIsMatch == 0) {
      uint litLen;
      uint i;

      if (!readLength(&bs, &litLen)) {
        free(payload);
        return false;
      }

      for (i = 0; i < litLen; i++) {
        byte b;
        if (!bsReadByte(&bs, &b) || !appendByte(&payload, &payloadSize, &payloadCap, b)) {
          free(payload);
          return false;
        }
      }

      needCopyBit = (litLen == 255) ? true : false;
      continue;
    }

    {
      uint lenField;
      uint offField;
      uint matchLen;
      uint offset;
      uint i;

      if (!readLength(&bs, &lenField)) {
        free(payload);
        return false;
      }

      if (lenField == 0xff) {
        break;
      }

      if (!readOffset(&bs, lenField, &offField)) {
        free(payload);
        return false;
      }

      matchLen = lenField + 1;
      offset = offField + 1;

      if ((size_t)offset > payloadSize) {
        free(payload);
        return false;
      }

      for (i = 0; i < matchLen; i++) {
        byte v = payload[payloadSize - offset];
        if (!appendByte(&payload, &payloadSize, &payloadCap, v)) {
          free(payload);
          return false;
        }
      }

      needCopyBit = true;
    }
  }

  *outPayload = payload;
  *outPayloadSize = payloadSize;
  return true;
}

static bool compareBuffers(const byte *a, size_t aSize, const byte *b, size_t bSize, size_t *diffOffset)
{
  size_t i;
  size_t minSize = (aSize < bSize) ? aSize : bSize;

  for (i = 0; i < minSize; i++) {
    if (a[i] != b[i]) {
      *diffOffset = i;
      return false;
    }
  }

  if (aSize != bSize) {
    *diffOffset = minSize;
    return false;
  }

  return true;
}

int decrunchToB3(const File *packedFile, const char *inputName, File *outB3File)
{
  byte *payload = NULL;
  size_t payloadSize = 0;
  byte *roundTripData = NULL;
  size_t roundTripSize = 0;
  char *b3Name;

  outB3File->name = NULL;
  outB3File->data = NULL;
  outB3File->size = 0;

  if (packedFile->size < 8) {
    printf("Error (B-3): Packed file too small (missing header).\n");
    return -1;
  }

  uint32_t loadAddr  = ((uint32_t)packedFile->data[0])       |
                       ((uint32_t)packedFile->data[1] <<  8) |
                       ((uint32_t)packedFile->data[2] << 16) |
                       ((uint32_t)packedFile->data[3] << 24);
  uint32_t origSize  = ((uint32_t)packedFile->data[4])       |
                       ((uint32_t)packedFile->data[5] <<  8) |
                       ((uint32_t)packedFile->data[6] << 16) |
                       ((uint32_t)packedFile->data[7] << 24);

  printf("[Decrunch] Input .b2 size:  0x%08X bytes\n", (unsigned int)packedFile->size);
  printf("[Decrunch] Load address:    0x%08X\n", loadAddr);
  printf("[Decrunch] Original size:   0x%08X bytes\n", origSize);
  printf("[Decrunch] Stream size:     0x%08X bytes\n", (unsigned int)(packedFile->size - 8));

  if (!decrunchPayload(packedFile->data + 8, packedFile->size - 8, &payload, &payloadSize)) {
    printf("Error (B-4): Decrunch failed for \"%s\".\n", packedFile->name);
    return -1;
  }

  roundTripSize = payloadSize;
  roundTripData = (byte *)malloc(roundTripSize);
  if (roundTripData == NULL) {
    printf("Error (B-5): Out of memory while building round-trip data.\n");
    free(payload);
    return -1;
  }

  memcpy(roundTripData, payload, payloadSize);

  b3Name = makeSuffixedName(inputName, ".b3");
  if (b3Name == NULL) {
    printf("Error (B-6): Could not allocate .b3 file name.\n");
    free(roundTripData);
    free(payload);
    return -1;
  }

  if (!writeRawFile(b3Name, roundTripData, roundTripSize)) {
    printf("Error (B-7): Could not write \"%s\".\n", b3Name);
    free(b3Name);
    free(roundTripData);
    free(payload);
    return -1;
  }

  outB3File->name = b3Name;
  outB3File->data = roundTripData;
  outB3File->size = roundTripSize;

  printf("[Decrunch] Output payload:  0x%08X bytes\n", (unsigned int)payloadSize);
  printf("[Decrunch] Output .b3 size: 0x%08X bytes\n", (unsigned int)roundTripSize);

  free(payload);
  return 0;
}

int validateOriginalVsB3(const File *originalFile, const File *b3File)
{
  size_t diffOffset = 0;

  printf("[Validate] Original size:   0x%08X bytes\n", (unsigned int)originalFile->size);
  printf("[Validate] Round-trip size: 0x%08X bytes\n", (unsigned int)b3File->size);

  if (compareBuffers(originalFile->data, originalFile->size,
                     b3File->data, b3File->size, &diffOffset)) {
    printf("Validation OK: \"%s\" matches original input.\n", b3File->name);
    printf("[Validate] Compared bytes:  0x%08X\n", (unsigned int)originalFile->size);
    return 0;
  }

  printf("Validation FAILED: \"%s\" differs from original at byte %zu.\n",
         b3File->name, diffOffset);
  if (diffOffset < originalFile->size && diffOffset < b3File->size) {
    printf("Expected 0x%02X, got 0x%02X\n",
           (unsigned)originalFile->data[diffOffset],
           (unsigned)b3File->data[diffOffset]);
  }

  return 2;
}
