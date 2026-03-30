#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#include "file.h"
#include "cruncher.h"
#include "decruncher.h"

int main(int argc, char *argv[])
{
	printf("argc = %d\n", argc);

	if (argc == 1)
	{
		printf("RCPacker [packer for Mega65] usage:\n");
		printf("\trcpacker <input_file>\n");
	}
	else
	{
		File myFile;
		File myBBFile;
		File b3File;
		char *fileName;
		int exitCode = 0;

		b3File.name = NULL;
		b3File.data = NULL;
		b3File.size = 0;

		fileName = argv[1];

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

		if (!writeFile(&myBBFile, myFile.name, 0))
		{
			printf("Error (B-2): Write file \"%s\", aborting.\n", myBBFile.name);
			freeFile(&myFile);
			freeFile(&myBBFile);
			return -1;
		}

		printf("ByteBoozer: \"%s\" -> \"%s\"\n", myFile.name, myBBFile.name);

		exitCode = decrunchToB3(&myBBFile, myFile.name, &b3File);
		if (exitCode != 0)
		{
			freeFile(&myFile);
			freeFile(&myBBFile);
			return exitCode;
		}

		exitCode = validateOriginalVsB3(&myFile, &b3File);

		freeFile(&myFile);
		freeFile(&myBBFile);
		freeFile(&b3File);

		return exitCode;
	}

	return 0;
}
