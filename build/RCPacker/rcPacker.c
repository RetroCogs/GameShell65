#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#include "file.h"
#include "cruncher.h"
#include "decruncher.h"
#include "validation.h"

int main(int argc, char *argv[])
{
	int doValidate = 0;
	char *fileName = NULL;
	int argi;

	printf("argc = %d\n", argc);

	if (argc == 1)
	{
		printf("RCPacker [packer for Mega65] usage:\n");
		printf("\trcpacker <input_file> [-v]\n");
		printf("\t-v  decrunch and validate after packing\n");
		return 0;
	}

	for (argi = 1; argi < argc; argi++)
	{
		if (strcmp(argv[argi], "-v") == 0)
		{
			doValidate = 1;
		}
		else if (fileName == NULL)
		{
			fileName = argv[argi];
		}
		else
		{
			printf("Error: Unknown argument \"%s\".\n", argv[argi]);
			printf("Usage: rcpacker <input_file> [-v]\n");
			return -1;
		}
	}

	if (fileName == NULL)
	{
		printf("Error: Missing input file.\n");
		printf("Usage: rcpacker <input_file> [-v]\n");
		return -1;
	}

	{
		File myFile;
		Buffer myBBBuffer;
		int exitCode = 0;
		size_t outNameLen = strlen(fileName) + 4;
		char *outName = (char *)malloc(outNameLen);

		if (outName == NULL)
		{
			printf("Error: Out of memory while building output file name.\n");
			return -1;
		}

		strncpy(outName, fileName, outNameLen - 4);
		strncpy(outName + (outNameLen - 4), ".b2", 4);

		if (!readFile(&myFile, fileName))
		{
			printf("Error (B-1): Open file \"%s\", aborting.\n", fileName);
			return -1;
		}

		if (!crunch(&myFile, &myBBBuffer))
		{
			freeFile(&myFile);
			free(outName);
			return -1;
		}

		if (!writeFile(&myBBBuffer, outName))
		{
			printf("Error (B-2): Write file \"%s\", aborting.\n", outName);
			freeFile(&myFile);
			free(myBBBuffer.data);
			free(outName);
			return -1;
		}

		printf("ByteBoozer: \"%s\" -> \"%s\"\n", myFile.name, outName);

		if (doValidate)
		{
			Buffer b3Buffer;

			b3Buffer.data = NULL;
			b3Buffer.size = 0;

			exitCode = decrunchToMemory(&myBBBuffer, &b3Buffer);
			if (exitCode == 0)
			{
				exitCode = validateBuffers(&myFile.buffer, &b3Buffer);
			}

			free(b3Buffer.data);
		}

		freeFile(&myFile);
		free(myBBBuffer.data);
		free(outName);

		return exitCode;
	}
}
