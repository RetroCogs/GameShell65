#include "file.h"
#include <stdlib.h>
#include <string.h>

void freeFile(File *aFile)
{
	free(aFile->name);
	free(aFile->buffer.data);
}

char *mystrdup(const char *str)
{
	// NOTE: Using manual malloc+strcpy instead of strdup()
	// strdup() has a bug in glibc 2.39 (Ubuntu 24.04) that causes it to
	// return invalid addresses when called in this context.
	// This manual approach is more portable and works on all platforms.
	char *dup = (char *)malloc(strlen(str) + 1);
	if (dup == NULL)
	{
		return NULL;
	}
	strcpy(dup, str);

	return dup;
}

bool readFile(File *aFile, const char *fileName)
{
	FILE *fp = NULL;
	struct stat fileStatus;

	aFile->name = mystrdup(fileName);

	if (stat(aFile->name, &fileStatus) == -1)
	{
		return false;
	}
	aFile->buffer.size = fileStatus.st_size;

	fp = fopen(aFile->name, "rb");
	if (fp == NULL)
	{
		return false;
	}

	aFile->buffer.data = (byte *)malloc(aFile->buffer.size);
	if (aFile->buffer.data == NULL)
	{
		fclose(fp);
		return false;
	}

	if (fread(aFile->buffer.data, 1, aFile->buffer.size, fp) != aFile->buffer.size)
	{
		fclose(fp);
		free(aFile->buffer.data);
		return false;
	}

	fclose(fp);
	return true;
}

bool writeFile(const Buffer *aBuffer, const char *fileName)
{
	FILE *fp = NULL;

	fp = fopen(fileName, "wb");
	if (fp == NULL)
	{
		return false;
	}

	if (fwrite(aBuffer->data, 1, aBuffer->size, fp) != aBuffer->size)
	{
		fclose(fp);
		return false;
	}

	fclose(fp);
	return true;
}
