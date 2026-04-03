#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <vector>
#include <string>

#include "file.h"
#include "cruncher.h"
#include "decruncher.h"
#include "validation.h"

int g_rcpackerQuiet = 0;

static void printUsage()
{
	printf("\n");
	printf("Usage: rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p <bytes>] [-q] [-v]\n");
	printf("  <input_file>     One or more files to pack together in the order given.\n");
	printf("  -o <output_file> Write the packed output to this filename.\n");
	printf("  -p <bytes>       Add zero padding between files so each next file starts on a <bytes> boundary.\n");
	printf("  -q               Quiet mode. Hide detailed crunch/decrunch output but keep input lines and the final summary.\n");
	printf("  -v               Decrunch the result and verify it matches the original input data.\n");
}

int main(int argc, char *argv[])
{
	int doValidate = 0;
	int doPadding = 0;
	char *outputNameArg = NULL;
	std::vector<std::string> inputFiles;
	int argi;

	printf("RCPacker - ByteBoozer2.0 cruncher.\n");

	if (argc == 1)
	{
		printUsage();
		return 0;
	}

	for (argi = 1; argi < argc; argi++)
	{
		if (strcmp(argv[argi], "-p") == 0)
		{
			char *endPtr;
			unsigned long paddingValue;

			if ((argi + 1) >= argc)
			{
				printf("\n");
				printf("Error: You used -p but did not provide a padding value.\n");
				printf("Please add a value after -p, for example: -p 256\n");
				printUsage();
				return -1;
			}

			paddingValue = strtoul(argv[++argi], &endPtr, 0);
			if (*argv[argi] == '\0' || *endPtr != '\0' || paddingValue == 0 || paddingValue > INT_MAX)
			{
				printf("\n");
				printf("Error: Invalid padding value \"%s\" for -p.\n", argv[argi]);
				printf("Use a positive integer value, for example: -p 256\n");
				printUsage();
				return -1;
			}

			doPadding = (int)paddingValue;
		}
		else if (strcmp(argv[argi], "-o") == 0)
		{
			if ((argi + 1) >= argc)
			{
				printf("\n");
				printf("Error: You used -o but did not give rcPacker an output filename after it.\n");
				printf("Please add a filename after -o, for example: rcpacker data.bin -o data.b2\n");
				printUsage();
				return -1;
			}
			outputNameArg = argv[++argi];
		}
		else if (strcmp(argv[argi], "-v") == 0)
		{
			doValidate = 1;
		}
		else if (strcmp(argv[argi], "-q") == 0)
		{
			g_rcpackerQuiet = 1;
		}
		else if (argv[argi][0] == '-')
		{
			printf("\n");
			printf("Error: rcPacker does not understand the option \"%s\".\n", argv[argi]);
			printf("Check the spelling of the option, or remove it if you did not mean to use it.\n");
			printUsage();
			return -1;
		}
		else
		{
			inputFiles.push_back(argv[argi]);
		}
	}

	if (inputFiles.empty())
	{
		printf("\n");
		printf("Error: You did not give rcPacker any input files to pack.\n");
		printf("Please provide at least one input file on the command line.\n");
		printUsage();
		return -1;
	}

	{
		File sourceFile;
		Buffer myBBBuffer;
		int exitCode = 0;
		double packedRatio = 0.0;
		size_t totalInputBytes = 0;
		size_t totalPaddingBytes = 0;
		size_t outNameLen;
		char *outName = NULL;
		size_t fileIndex;

		sourceFile.name = NULL;
		sourceFile.buffer.data = NULL;
		sourceFile.buffer.size = 0;
		myBBBuffer.data = NULL;
		myBBBuffer.size = 0;

		if (outputNameArg != NULL)
		{
			outNameLen = strlen(outputNameArg) + 1;
		}
		else
		{
			outNameLen = inputFiles[0].size() + 4;
		}

		outName = (char *)malloc(outNameLen);

		if (outName == NULL)
		{
			printf("\n");
			printf("Error: rcPacker ran out of memory while preparing the output filename.\n");
			printf("Try again with fewer inputs or free some system memory and retry.\n");
			return -1;
		}

		if (outputNameArg != NULL)
		{
			strcpy(outName, outputNameArg);
		}
		else
		{
			strncpy(outName, inputFiles[0].c_str(), outNameLen - 4);
			strncpy(outName + (outNameLen - 4), ".b2", 4);
		}

		if (!g_rcpackerQuiet)
		{
			printf("\n");
		}

		for (fileIndex = 0; fileIndex < inputFiles.size(); fileIndex++)
		{
			File partFile;
			size_t padBytes = 0;
			size_t startOffset = sourceFile.buffer.size;
			size_t newSize;
			byte *newData;

			if (!readFile(&partFile, inputFiles[fileIndex].c_str()))
			{
				printf("\n");
				printf("Error: rcPacker could not open input file \"%s\".\n", inputFiles[fileIndex].c_str());
				printf("Make sure the file exists, the path is correct, and you have permission to read it.\n");
				free(sourceFile.buffer.data);
				free(outName);
				return -1;
			}

			if (doPadding > 0)
			{
				/* Pad this file's end so the next file starts at the requested boundary. */
				if ((fileIndex + 1) < inputFiles.size())
				{
					padBytes = ((size_t)doPadding - (partFile.buffer.size % (size_t)doPadding)) % (size_t)doPadding;
				}
			}

			printf("input(%02zu) start=$%08X  size=$%08X  pad=$%08X\t\"%s\"\n",
				fileIndex + 1,
				(unsigned int)startOffset,
				(unsigned int)partFile.buffer.size,
				(unsigned int)padBytes,
				inputFiles[fileIndex].c_str());

			newSize = sourceFile.buffer.size + partFile.buffer.size + padBytes;
			newData = (byte *)realloc(sourceFile.buffer.data, newSize);
			if (newData == NULL)
			{
				printf("\n");
				printf("Error: rcPacker ran out of memory while combining the input files.\n");
				printf("This usually means the total input data is larger than the available memory for packing.\n");
				freeFile(&partFile);
				free(sourceFile.buffer.data);
				free(outName);
				return -1;
			}

			sourceFile.buffer.data = newData;
			memcpy(sourceFile.buffer.data + sourceFile.buffer.size,
				partFile.buffer.data,
				partFile.buffer.size);
			if (padBytes > 0)
			{
				/* Keep padding deterministic and highly compressible. */
				memset(sourceFile.buffer.data + sourceFile.buffer.size + partFile.buffer.size, 0, padBytes);
			}
			sourceFile.buffer.size = newSize;
			totalInputBytes += partFile.buffer.size;
			totalPaddingBytes += padBytes;

			freeFile(&partFile);
		}

		if (!g_rcpackerQuiet)
		{
			printf("\n");
		}

		if (!crunch(&sourceFile, &myBBBuffer))
		{
			free(sourceFile.buffer.data);
			free(outName);
			return -1;
		}

		if (!g_rcpackerQuiet)
		{
			printf("\n");
		}

		if (!writeFile(&myBBBuffer, outName))
		{
			printf("\n");
			printf("Error: rcPacker could not write the output file \"%s\".\n", outName);
			printf("Check that the destination folder exists and that you have permission to write there.\n");
			free(sourceFile.buffer.data);
			free(myBBBuffer.data);
			free(outName);
			return -1;
		}

		if (sourceFile.buffer.size > 0)
		{
			packedRatio = (double)myBBBuffer.size * 100.0 / (double)sourceFile.buffer.size;
		}

		if (!g_rcpackerQuiet)
		{
			printf("ByteBoozer summary:\n");
			printf("  files         : $%08X\n", (unsigned int)inputFiles.size());
			printf("  source bytes  : $%08X\n", (unsigned int)totalInputBytes);
			printf("  padding bytes : $%08X\n", (unsigned int)totalPaddingBytes);
			printf("  packed bytes  : $%08X\n", (unsigned int)myBBBuffer.size);
			if (sourceFile.buffer.size > 0)
			{
				printf("  packed ratio  : %.2f%%\n", packedRatio);
			}
			printf("  output        : \"%s\"\n", outName);
		}

		if (doValidate)
		{
			Buffer b3Buffer;

			b3Buffer.data = NULL;
			b3Buffer.size = 0;

			if (!g_rcpackerQuiet)
			{
				printf("\n");
				printf("Validating round-trip decrunch output against original input...\n");
			}

			exitCode = decrunchToMemory(&myBBBuffer, &b3Buffer);
			if (exitCode == 0)
			{
				exitCode = validateBuffers(&sourceFile.buffer, &b3Buffer);
			}

			free(b3Buffer.data);
		}

		if (g_rcpackerQuiet)
		{
			printf("output   ");
			printf(" summary source=$%08X packed=$%08X",
				(unsigned int)totalInputBytes,
				(unsigned int)myBBBuffer.size);
			if (sourceFile.buffer.size > 0)
			{
				printf(" ratio=%.2f", packedRatio);
			}
			printf("\t\"%s\"", outName);
		}

		free(sourceFile.buffer.data);
		free(myBBBuffer.data);
		free(outName);

		return exitCode;
	}
}
