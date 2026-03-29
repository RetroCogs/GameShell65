#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef unsigned char byte;
typedef unsigned int uint;

typedef struct {
  byte *data;
  size_t size;
} Buffer;

typedef struct {
  const byte *src;
  size_t srcSize;
  size_t srcPos;
  byte curBitByte;
  uint bitsLeft;
} BitStream;

static int read_file(const char *path, Buffer *buf) {
  FILE *fp;
  long fileSize;
  size_t n;

  fp = fopen(path, "rb");
  if (fp == NULL) {
    return 0;
  }

  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    return 0;
  }

  fileSize = ftell(fp);
  if (fileSize < 0) {
    fclose(fp);
    return 0;
  }

  if (fseek(fp, 0, SEEK_SET) != 0) {
    fclose(fp);
    return 0;
  }

  buf->size = (size_t)fileSize;
  buf->data = (byte *)malloc(buf->size);
  if (buf->data == NULL && buf->size != 0) {
    fclose(fp);
    return 0;
  }

  n = fread(buf->data, 1, buf->size, fp);
  fclose(fp);

  if (n != buf->size) {
    free(buf->data);
    buf->data = NULL;
    buf->size = 0;
    return 0;
  }

  return 1;
}

static void free_buffer(Buffer *buf) {
  free(buf->data);
  buf->data = NULL;
  buf->size = 0;
}

static uint32_t read_le32(const byte *p) {
  return ((uint32_t)p[0]) |
         ((uint32_t)p[1] << 8) |
         ((uint32_t)p[2] << 16) |
         ((uint32_t)p[3] << 24);
}

static int bs_read_byte(BitStream *bs, byte *out) {
  if (bs->srcPos >= bs->srcSize) {
    return 0;
  }

  *out = bs->src[bs->srcPos++];
  return 1;
}

static int bs_read_bit(BitStream *bs, uint *outBit) {
  if (bs->bitsLeft == 0) {
    if (!bs_read_byte(bs, &bs->curBitByte)) {
      return 0;
    }
    bs->bitsLeft = 8;
  }

  *outBit = (bs->curBitByte & 0x80) ? 1u : 0u;
  bs->curBitByte <<= 1;
  bs->bitsLeft--;
  return 1;
}

static int read_length(BitStream *bs, uint *outLen) {
  uint len = 1;
  uint b;

  while (1) {
    if (!bs_read_bit(bs, &b)) {
      return 0;
    }

    if (b == 0) {
      *outLen = len;
      return 1;
    }

    if (!bs_read_bit(bs, &b)) {
      return 0;
    }

    len = (len << 1) | b;

    // For lengths >= 128 there is no trailing 0 terminator in this format.
    if (len >= 128) {
      *outLen = len;
      return 1;
    }
  }
}

static uint bits_for_offset_group(uint group, uint lenField) {
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

static int read_offset(BitStream *bs, uint lenField, uint *outOffset) {
  uint b0;
  uint b1;
  uint group;
  uint n;
  uint value = 0;
  uint mask;
  byte low;

  if (!bs_read_bit(bs, &b0) || !bs_read_bit(bs, &b1)) {
    return 0;
  }

  group = (b0 << 1) | b1;
  n = bits_for_offset_group(group, lenField);
  if (n == 0) {
    return 0;
  }

  if (n >= 8) {
    mask = (1u << n);
    while (mask > 0x100) {
      uint bit;
      mask >>= 1;
      if (!bs_read_bit(bs, &bit)) {
        return 0;
      }
      if (bit) {
        value |= mask;
      }
    }

    if (!bs_read_byte(bs, &low)) {
      return 0;
    }
    value |= (uint)(low ^ 255u);
  } else {
    mask = (1u << n);
    while (mask > 1) {
      uint bit;
      mask >>= 1;
      if (!bs_read_bit(bs, &bit)) {
        return 0;
      }
      // Inverted for short offsets in writer.
      if (bit == 0) {
        value |= mask;
      }
    }
  }

  *outOffset = value;
  return 1;
}

static int append_byte(Buffer *out, size_t *cap, byte v) {
  byte *newMem;
  size_t newCap;

  if (out->size < *cap) {
    out->data[out->size++] = v;
    return 1;
  }

  newCap = (*cap == 0) ? 1024 : (*cap * 2);
  newMem = (byte *)realloc(out->data, newCap);
  if (newMem == NULL) {
    return 0;
  }

  out->data = newMem;
  *cap = newCap;
  out->data[out->size++] = v;
  return 1;
}

static int decrunch_stream(const byte *packed, size_t packedSize, Buffer *outPayload) {
  BitStream bs;
  size_t cap = 0;
  int needCopyBit = 1;
  size_t tokenCount = 0;

  outPayload->data = NULL;
  outPayload->size = 0;

  bs.src = packed;
  bs.srcSize = packedSize;
  bs.srcPos = 0;
  bs.curBitByte = 0;
  bs.bitsLeft = 0;

  while (1) {
    uint tokenIsMatch = 1;
    tokenCount++;

    if (needCopyBit) {
      if (!bs_read_bit(&bs, &tokenIsMatch)) {
        printf("Decode error: could not read copy bit at token %zu, stream byte %zu\n", tokenCount, bs.srcPos);
        return 0;
      }
    }

    if (tokenIsMatch == 0) {
      uint litLen;
      uint i;

      if (!read_length(&bs, &litLen)) {
        printf("Decode error: could not read literal length at token %zu, stream byte %zu\n", tokenCount, bs.srcPos);
        return 0;
      }

      for (i = 0; i < litLen; i++) {
        byte b;
        if (!bs_read_byte(&bs, &b)) {
          printf("Decode error: literal byte underflow at token %zu, lit index %u, stream byte %zu\n", tokenCount, i, bs.srcPos);
          return 0;
        }
        if (!append_byte(outPayload, &cap, b)) {
          return 0;
        }
      }

      needCopyBit = (litLen == 255) ? 1 : 0;
      continue;
    }

    // Match or end marker
    {
      uint lenField;
      uint offField;
      uint matchLen;
      uint offset;
      uint i;

      if (!read_length(&bs, &lenField)) {
        printf("Decode error: could not read match length at token %zu, stream byte %zu\n", tokenCount, bs.srcPos);
        return 0;
      }

      if (lenField == 0xff) {
        return 1;
      }

      if (!read_offset(&bs, lenField, &offField)) {
        printf("Decode error: could not read match offset at token %zu, lenField %u, stream byte %zu\n", tokenCount, lenField, bs.srcPos);
        return 0;
      }

      matchLen = lenField + 1;
      offset = offField + 1;

      if ((size_t)offset > outPayload->size) {
        printf("Decode error: back-reference out of range at token %zu (offset=%u, outSize=%zu)\n", tokenCount, offset, outPayload->size);
        return 0;
      }

      for (i = 0; i < matchLen; i++) {
        byte v = outPayload->data[outPayload->size - offset];
        if (!append_byte(outPayload, &cap, v)) {
          return 0;
        }
      }

      needCopyBit = 1;
    }
  }
}

static int compare_buffers(const Buffer *a, const Buffer *b, size_t *diffAt) {
  size_t minSize;
  size_t i;

  minSize = (a->size < b->size) ? a->size : b->size;
  for (i = 0; i < minSize; i++) {
    if (a->data[i] != b->data[i]) {
      *diffAt = i;
      return 0;
    }
  }

  if (a->size != b->size) {
    *diffAt = minSize;
    return 0;
  }

  return 1;
}

int main(int argc, char **argv) {
  Buffer packed;
  Buffer original;
  Buffer payload;
  Buffer reconstructed;
  uint32_t depackAddr;
  size_t diffAt = 0;

  packed.data = NULL;
  packed.size = 0;
  original.data = NULL;
  original.size = 0;
  payload.data = NULL;
  payload.size = 0;
  reconstructed.data = NULL;
  reconstructed.size = 0;

  if (argc != 3) {
    printf("Usage: decruncher <packed_file.b2> <original_file>\n");
    printf("Expects non-executable rcpacker format: [packed_addr32][depack_addr32][packed_stream]\n");
    return 1;
  }

  if (!read_file(argv[1], &packed)) {
    printf("Error: could not read packed file: %s\n", argv[1]);
    return 1;
  }

  if (!read_file(argv[2], &original)) {
    printf("Error: could not read original file: %s\n", argv[2]);
    free_buffer(&packed);
    return 1;
  }

  if (packed.size < 8) {
    printf("Error: packed file too small (%zu bytes), expected at least 8-byte header\n", packed.size);
    free_buffer(&packed);
    free_buffer(&original);
    return 1;
  }

  depackAddr = read_le32(&packed.data[4]);

  if (!decrunch_stream(&packed.data[8], packed.size - 8, &payload)) {
    printf("Error: decrunch failed (invalid or truncated stream)\n");
    free_buffer(&packed);
    free_buffer(&original);
    free_buffer(&payload);
    return 1;
  }

  reconstructed.size = payload.size + 4;
  reconstructed.data = (byte *)malloc(reconstructed.size);
  if (reconstructed.data == NULL) {
    printf("Error: out of memory while building reconstructed file\n");
    free_buffer(&packed);
    free_buffer(&original);
    free_buffer(&payload);
    return 1;
  }

  reconstructed.data[0] = (byte)(depackAddr & 0xff);
  reconstructed.data[1] = (byte)((depackAddr >> 8) & 0xff);
  reconstructed.data[2] = (byte)((depackAddr >> 16) & 0xff);
  reconstructed.data[3] = (byte)((depackAddr >> 24) & 0xff);
  memcpy(&reconstructed.data[4], payload.data, payload.size);

  if (compare_buffers(&reconstructed, &original, &diffAt)) {
    printf("OK: decrunched output matches original.\n");
    printf("Original size:     %zu bytes\n", original.size);
    printf("Packed size:       %zu bytes\n", packed.size);
    printf("Decrunched payload %zu bytes\n", payload.size);
    free_buffer(&packed);
    free_buffer(&original);
    free_buffer(&payload);
    free_buffer(&reconstructed);
    return 0;
  }

  printf("MISMATCH: decrunched output differs from original at byte offset %zu\n", diffAt);
  if (diffAt < reconstructed.size && diffAt < original.size) {
    printf("Expected 0x%02X, got 0x%02X\n", (unsigned)original.data[diffAt], (unsigned)reconstructed.data[diffAt]);
  }
  printf("Original size:     %zu bytes\n", original.size);
  printf("Reconstructed size %zu bytes\n", reconstructed.size);

  free_buffer(&packed);
  free_buffer(&original);
  free_buffer(&payload);
  free_buffer(&reconstructed);
  return 2;
}