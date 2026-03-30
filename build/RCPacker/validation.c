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

int validateOriginalVsB3(const File *originalFile, const File *b3File)
{
	size_t diffOffset = 0;

	printf("[Validate] Original size:   0x%08X bytes\n", (unsigned int)originalFile->size);
	printf("[Validate] Round-trip size: 0x%08X bytes\n", (unsigned int)b3File->size);

	if (compareBuffers(originalFile->data, originalFile->size,
				b3File->data, b3File->size, &diffOffset))
	{
		printf("Validation OK: \"%s\" matches original input.\n", b3File->name);
		printf("[Validate] Compared bytes:  0x%08X\n", (unsigned int)originalFile->size);
		return 0;
	}

	printf("Validation FAILED: \"%s\" differs from original at byte %zu.\n",
		b3File->name, diffOffset);
	if (diffOffset < originalFile->size && diffOffset < b3File->size)
	{
		printf("Expected 0x%02X, got 0x%02X\n",
			(unsigned)originalFile->data[diffOffset],
			(unsigned)b3File->data[diffOffset]);
	}

	return 2;
}
