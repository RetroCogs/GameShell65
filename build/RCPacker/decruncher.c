#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "decruncher.h"

typedef struct
{
	const byte *src;
	size_t srcSize;
	size_t srcPos;
	byte curBitByte;
	uint bitsLeft;
} BitStream;

static bool bsReadByte(BitStream *bs, byte *out)
{
	if (bs->srcPos >= bs->srcSize)
	{
		return false;
	}

	*out = bs->src[bs->srcPos++];
	return true;
}

static bool bsReadBit(BitStream *bs, uint *outBit)
{
	if (bs->bitsLeft == 0)
	{
		if (!bsReadByte(bs, &bs->curBitByte))
		{
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

	while (1)
	{
		uint b;

		if (!bsReadBit(bs, &b))
		{
			return false;
		}

		if (b == 0)
		{
			*outLen = len;
			return true;
		}

		if (!bsReadBit(bs, &b))
		{
			return false;
		}

		len = (len << 1) | b;

		if (len >= 128)
		{
			*outLen = len;
			return true;
		}
	}
}

static uint bitsForOffsetGroup(uint group, uint lenField)
{
	static const uint shortBits[4] = {3, 6, 8, 10};
	static const uint longBits[4] = {4, 7, 10, 13};

	if (group > 3)
	{
		return 0;
	}

	if (lenField == 1)
	{
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

	if (!bsReadBit(bs, &b0) || !bsReadBit(bs, &b1))
	{
		return false;
	}

	group = (b0 << 1) | b1;
	n = bitsForOffsetGroup(group, lenField);
	if (n == 0)
	{
		return false;
	}

	if (n >= 8)
	{
		byte low;

		mask = (1u << n);
		while (mask > 0x100)
		{
			uint bit;
			mask >>= 1;
			if (!bsReadBit(bs, &bit))
			{
				return false;
			}
			if (bit)
			{
				value |= mask;
			}
		}

		if (!bsReadByte(bs, &low))
		{
			return false;
		}
		value |= (uint)(low ^ 255u);
	}
	else
	{
		mask = (1u << n);
		while (mask > 1)
		{
			uint bit;
			mask >>= 1;
			if (!bsReadBit(bs, &bit))
			{
				return false;
			}
			if (bit == 0)
			{
				value |= mask;
			}
		}
	}

	*outOffset = value;
	return true;
}

static bool appendByte(byte **out, size_t *outSize, size_t *cap, byte v)
{
	if (*outSize >= *cap)
	{
		byte *newData;
		size_t newCap = (*cap == 0) ? 1024 : (*cap * 2);
		newData = (byte *)realloc(*out, newCap);
		if (newData == NULL)
		{
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

	while (1)
	{
		uint tokenIsMatch = 1;

		if (needCopyBit)
		{
			if (!bsReadBit(&bs, &tokenIsMatch))
			{
				free(payload);
				return false;
			}
		}

		if (tokenIsMatch == 0)
		{
			uint litLen;
			uint i;

			if (!readLength(&bs, &litLen))
			{
				free(payload);
				return false;
			}

			for (i = 0; i < litLen; i++)
			{
				byte b;
				if (!bsReadByte(&bs, &b) || !appendByte(&payload, &payloadSize, &payloadCap, b))
				{
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

			if (!readLength(&bs, &lenField))
			{
				free(payload);
				return false;
			}

			if (lenField == 0xff)
			{
				break;
			}

			if (!readOffset(&bs, lenField, &offField))
			{
				free(payload);
				return false;
			}

			matchLen = lenField + 1;
			offset = offField + 1;

			if ((size_t)offset > payloadSize)
			{
				free(payload);
				return false;
			}

			for (i = 0; i < matchLen; i++)
			{
				byte v = payload[payloadSize - offset];
				if (!appendByte(&payload, &payloadSize, &payloadCap, v))
				{
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

int decrunchToMemory(const Buffer *packedBuffer, Buffer *outBuffer)
{
	byte *payload = NULL;
	size_t payloadSize = 0;

	outBuffer->data = NULL;
	outBuffer->size = 0;

	if (packedBuffer->size < 8)
	{
		printf("Error (B-3): Packed file too small (missing header).\n");
		return -1;
	}

	uint32_t loadAddr = ((uint32_t)packedBuffer->data[0]) |
		((uint32_t)packedBuffer->data[1] << 8) |
		((uint32_t)packedBuffer->data[2] << 16) |
		((uint32_t)packedBuffer->data[3] << 24);
	uint32_t origSize = ((uint32_t)packedBuffer->data[4]) |
		((uint32_t)packedBuffer->data[5] << 8) |
		((uint32_t)packedBuffer->data[6] << 16) |
		((uint32_t)packedBuffer->data[7] << 24);

	printf("[Decrunch] Input .b2 size:  0x%08X bytes\n", (unsigned int)packedBuffer->size);
	printf("[Decrunch] Load address:    0x%08X\n", loadAddr);
	printf("[Decrunch] Original size:   0x%08X bytes\n", origSize);
	printf("[Decrunch] Stream size:     0x%08X bytes\n", (unsigned int)(packedBuffer->size - 8));

	if (!decrunchPayload(packedBuffer->data + 8, packedBuffer->size - 8, &payload, &payloadSize))
	{
		printf("Error (B-4): Decrunch failed.\n");
		return -1;
	}

	outBuffer->data = payload;
	outBuffer->size = payloadSize;

	printf("[Decrunch] Output payload:  0x%08X bytes\n", (unsigned int)payloadSize);
	printf("[Decrunch] Output size: 0x%08X bytes\n", (unsigned int)payloadSize);
	printf("[Decrunch] Validation path: in-memory\n");

	return 0;
}

	