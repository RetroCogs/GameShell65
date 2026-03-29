#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>

#include "file.h"
#include "cruncher.h"

typedef struct {
    const byte *src;
    size_t srcSize;
    size_t srcPos;
    byte curBitByte;
    uint bitsLeft;
} BitStream;

static uint32_t read_le32(const byte *p)
{
    return ((uint32_t)p[0]) |
           ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) |
           ((uint32_t)p[3] << 24);
}

static bool write_raw_file(const char *fileName, const byte *data, size_t size)
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

static char *make_suffixed_name(const char *fileName, const char *suffix)
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

static bool bs_read_byte(BitStream *bs, byte *out)
{
    if (bs->srcPos >= bs->srcSize) {
        return false;
    }

    *out = bs->src[bs->srcPos++];
    return true;
}

static bool bs_read_bit(BitStream *bs, uint *outBit)
{
    if (bs->bitsLeft == 0) {
        if (!bs_read_byte(bs, &bs->curBitByte)) {
            return false;
        }
        bs->bitsLeft = 8;
    }

    *outBit = (bs->curBitByte & 0x80) ? 1u : 0u;
    bs->curBitByte <<= 1;
    bs->bitsLeft--;
    return true;
}

static bool read_length(BitStream *bs, uint *outLen)
{
    uint len = 1;

    while (1) {
        uint b;

        if (!bs_read_bit(bs, &b)) {
            return false;
        }

        if (b == 0) {
            *outLen = len;
            return true;
        }

        if (!bs_read_bit(bs, &b)) {
            return false;
        }

        len = (len << 1) | b;

        // For encoded values >= 128, no trailing 0 bit is emitted.
        if (len >= 128) {
            *outLen = len;
            return true;
        }
    }
}

static uint bits_for_offset_group(uint group, uint lenField)
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

static bool read_offset(BitStream *bs, uint lenField, uint *outOffset)
{
    uint b0;
    uint b1;
    uint group;
    uint n;
    uint value = 0;
    uint mask;

    if (!bs_read_bit(bs, &b0) || !bs_read_bit(bs, &b1)) {
        return false;
    }

    group = (b0 << 1) | b1;
    n = bits_for_offset_group(group, lenField);
    if (n == 0) {
        return false;
    }

    if (n >= 8) {
        byte low;

        mask = (1u << n);
        while (mask > 0x100) {
            uint bit;
            mask >>= 1;
            if (!bs_read_bit(bs, &bit)) {
                return false;
            }
            if (bit) {
                value |= mask;
            }
        }

        if (!bs_read_byte(bs, &low)) {
            return false;
        }
        value |= (uint)(low ^ 255u);
    } else {
        mask = (1u << n);
        while (mask > 1) {
            uint bit;
            mask >>= 1;
            if (!bs_read_bit(bs, &bit)) {
                return false;
            }
            // Short offset bits are inverted in cruncher.
            if (bit == 0) {
                value |= mask;
            }
        }
    }

    *outOffset = value;
    return true;
}

static bool append_byte(byte **out, size_t *outSize, size_t *cap, byte v)
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

static bool decrunch_payload(const byte *packed, size_t packedSize, byte **outPayload, size_t *outPayloadSize)
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
            if (!bs_read_bit(&bs, &tokenIsMatch)) {
                free(payload);
                return false;
            }
        }

        if (tokenIsMatch == 0) {
            uint litLen;
            uint i;

            if (!read_length(&bs, &litLen)) {
                free(payload);
                return false;
            }

            for (i = 0; i < litLen; i++) {
                byte b;
                if (!bs_read_byte(&bs, &b) || !append_byte(&payload, &payloadSize, &payloadCap, b)) {
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

            if (!read_length(&bs, &lenField)) {
                free(payload);
                return false;
            }

            if (lenField == 0xff) {
                break;
            }

            if (!read_offset(&bs, lenField, &offField)) {
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
                if (!append_byte(&payload, &payloadSize, &payloadCap, v)) {
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

static bool compare_files(const File *a, const File *b, size_t *diffOffset)
{
    size_t i;
    size_t minSize = (a->size < b->size) ? a->size : b->size;

    for (i = 0; i < minSize; i++) {
        if (a->data[i] != b->data[i]) {
            *diffOffset = i;
            return false;
        }
    }

    if (a->size != b->size) {
        *diffOffset = minSize;
        return false;
    }

    return true;
}

int main(int argc, char * argv[])
{
    printf("argc = %d\n", argc);

    if (argc == 1)
    {
        printf("RCPacker [packer for Mega65] usage:\n");
        printf("\trcpacker <input_file> [-e] [-r] [-c]\n");
        printf("\t  -e : treat as executable program\n");
        printf("\t  -r : apply relocation\n");
        printf("\t  -c : clip start address\n");
    }
    else
    {
        File myFile;
        File myBBFile;
                File roundTripFile;
        char* fileName;
        bool isExecutable = false;
        bool isRelocated = false;
                bool clipStartAddress = false;
        uint address = 0;
                byte *payload = NULL;
                size_t payloadSize = 0;
                byte *roundTripData = NULL;
                size_t roundTripSize = 0;
                char *b3Name = NULL;
                size_t diffOffset = 0;
                int exitCode = 0;

                roundTripFile.name = NULL;
                roundTripFile.data = NULL;
                roundTripFile.size = 0;

        fileName = argv[1];

        if(!readFile(&myFile, fileName)) {
            printf("Error (B-1): Open file \"%s\", aborting.\n", fileName);
            return -1;
        }

        if(!crunch(&myFile, &myBBFile, address, isExecutable, isRelocated)) {
            freeFile(&myFile);
            return -1;
        }

        if(!writeFile(&myBBFile, myFile.name, clipStartAddress == false ? 0 : 2)) {
            printf("Error (B-2): Write file \"%s\", aborting.\n", myBBFile.name);
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        printf("ByteBoozer: \"%s\" -> \"%s\"\n", myFile.name, myBBFile.name);

        if (isExecutable) {
            printf("Skipping .b3 round-trip validation for executable mode.\n");
            freeFile(&myFile);
            freeFile(&myBBFile);
            return 0;
        }

        if (myBBFile.size < 8) {
            printf("Error (B-3): Packed output too small for 32-bit data header.\n");
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        if (!decrunch_payload(myBBFile.data + 8, myBBFile.size - 8, &payload, &payloadSize)) {
            printf("Error (B-4): Decrunch failed for \"%s\".\n", myBBFile.name);
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        roundTripSize = payloadSize + 4;
        roundTripData = (byte *)malloc(roundTripSize);
        if (roundTripData == NULL) {
            printf("Error (B-5): Out of memory while building round-trip data.\n");
            free(payload);
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        {
            uint32_t depackAddr = read_le32(myBBFile.data + 4);
            roundTripData[0] = (byte)(depackAddr & 0xff);
            roundTripData[1] = (byte)((depackAddr >> 8) & 0xff);
            roundTripData[2] = (byte)((depackAddr >> 16) & 0xff);
            roundTripData[3] = (byte)((depackAddr >> 24) & 0xff);
        }

        memcpy(roundTripData + 4, payload, payloadSize);

        b3Name = make_suffixed_name(myFile.name, ".b3");
        if (b3Name == NULL) {
            printf("Error (B-6): Could not allocate .b3 file name.\n");
            free(roundTripData);
            free(payload);
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        if (!write_raw_file(b3Name, roundTripData, roundTripSize)) {
            printf("Error (B-7): Could not write \"%s\".\n", b3Name);
            free(b3Name);
            free(roundTripData);
            free(payload);
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        if(!readFile(&roundTripFile, b3Name)) {
            printf("Error (B-8): Could not re-open \"%s\" for validation.\n", b3Name);
            free(b3Name);
            free(roundTripData);
            free(payload);
            freeFile(&myFile);
            freeFile(&myBBFile);
            return -1;
        }

        if (compare_files(&myFile, &roundTripFile, &diffOffset)) {
            printf("Validation OK: \"%s\" matches original input.\n", b3Name);
        } else {
            printf("Validation FAILED: \"%s\" differs from original at byte %zu.\n", b3Name, diffOffset);
            if (diffOffset < myFile.size && diffOffset < roundTripFile.size) {
                printf("Expected 0x%02X, got 0x%02X\n",
                       (unsigned)myFile.data[diffOffset],
                       (unsigned)roundTripFile.data[diffOffset]);
            }
            exitCode = 2;
        }

        free(b3Name);
        free(roundTripData);
        free(payload);

        freeFile(&roundTripFile);

        freeFile(&myFile);
        freeFile(&myBBFile);

        return exitCode;

    }

    return 0;
}
