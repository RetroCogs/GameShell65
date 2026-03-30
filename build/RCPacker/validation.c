#include <stdio.h>
#include <string.h>

#include "file.h"

static bool compareBuffers(const byte *a, size_t aSize, const byte *b, size_t bSize, size_t *diffOffset)
{
	size_t i;
	size_t minSize = (aSize < bSize) ? aSize : bSize;

	for (i = 0; i < minSize; i++)
	{
		if (a[i] != b[i])
		{
			*diffOffset = i;
			return false;
		}
	}

	if (aSize != bSize)
	{
		*diffOffset = minSize;
		return false;
	}

	return true;
}

int validateBuffers(const Buffer *originalBuffer, const Buffer *roundTripBuffer)
{
	size_t diffOffset = 0;

	printf("[Validate] Original size:   0x%08X bytes\n", (unsigned int)originalBuffer->size);
	printf("[Validate] Round-trip size: 0x%08X bytes\n", (unsigned int)roundTripBuffer->size);

	if (compareBuffers(originalBuffer->data, originalBuffer->size,
				roundTripBuffer->data, roundTripBuffer->size, &diffOffset))
	{
		printf("Validation OK: round-trip data matches original input.\n");
		printf("[Validate] Compared bytes:  0x%08X\n", (unsigned int)originalBuffer->size);
		return 0;
	}

	printf("Validation FAILED: round-trip data differs from original at byte %zu.\n", diffOffset);
	if (diffOffset < originalBuffer->size && diffOffset < roundTripBuffer->size)
	{
		printf("Expected 0x%02X, got 0x%02X\n",
			(unsigned)originalBuffer->data[diffOffset],
			(unsigned)roundTripBuffer->data[diffOffset]);
	}

	return 2;
}
