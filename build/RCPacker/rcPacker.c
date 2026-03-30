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
		File myBBFile;
		File b3File;
		int exitCode = 0;

		b3File.name = NULL;
		b3File.data = NULL;
		b3File.size = 0;

		if (!readFile(&myFile, fileName))
		{
			printf("Error (B-1): Open file \"%s\", aborting.\n", fileName);
			return -1;
		}

		if (!crunch(&myFile, &myBBFile))
		{
			freeFile(&myFile);
			return -1;
		}

		if (!writeFile(&myBBFile, myFile.name))
		{
			printf("Error (B-2): Write file \"%s\", aborting.\n", myBBFile.name);
			freeFile(&myFile);
			freeFile(&myBBFile);
			return -1;
		}

		printf("ByteBoozer: \"%s\" -> \"%s\"\n", myFile.name, myBBFile.name);

		if (doValidate)
		{
			exitCode = decrunchToMemory(&myBBFile, myFile.name, &b3File);
			if (exitCode == 0)
			{
				exitCode = validateOriginalVsB3(&myFile, &b3File);
			}
		}

		freeFile(&myFile);
		freeFile(&myBBFile);
		if (doValidate)
		{
			freeFile(&b3File);
		}

		return exitCode;
	}
}
