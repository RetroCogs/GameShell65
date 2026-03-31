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

int main(int argc, char *argv[])
{
	int doValidate = 0;
	int doPadding = 0;
	char *outputNameArg = NULL;
	std::vector<std::string> inputFiles;
	int argi;

	if (argc == 1)
	{
		printf("RCPacker [packer for Mega65] usage:\n");
		printf("\trcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-v]\n");
		printf("\t-o  output file name (default: first input + .b2)\n");
		printf("\t-p  round each file start to 256 bytes\n");
		printf("\t-v  decrunch and validate after packing\n");
		return 0;
	}

	for (argi = 1; argi < argc; argi++)
	{
		if (strcmp(argv[argi], "-p") == 0)
		{
			doPadding = 0x100;
		}
		else if (strcmp(argv[argi], "-o") == 0)
		{
			if ((argi + 1) >= argc)
			{
				printf("Error: Missing filename after -o.\n");
				printf("Usage: rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-v]\n");
				return -1;
			}
			outputNameArg = argv[++argi];
		}
		else if (strcmp(argv[argi], "-v") == 0)
		{
			doValidate = 1;
		}
		else if (argv[argi][0] == '-')
		{
			printf("Error: Unknown argument \"%s\".\n", argv[argi]);
			printf("Usage: rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-v]\n");
			return -1;
		}
		else
		{
			inputFiles.push_back(argv[argi]);
		}
	}

	if (inputFiles.empty())
	{
		printf("Error: Missing input file.\n");
		printf("Usage: rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-v]\n");
		return -1;
	}

	{
		File sourceFile;
		Buffer myBBBuffer;
		int exitCode = 0;
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
			printf("Error: Out of memory while building output file name.\n");
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

		for (fileIndex = 0; fileIndex < inputFiles.size(); fileIndex++)
		{
			File partFile;
			size_t padBytes = 0;
			size_t startOffset = sourceFile.buffer.size;
			size_t newSize;
			byte *newData;

			if (!readFile(&partFile, inputFiles[fileIndex].c_str()))
			{
				printf("Error (B-1): Open file \"%s\", aborting.\n", inputFiles[fileIndex].c_str());
				free(sourceFile.buffer.data);
				free(outName);
				return -1;
			}

			if (doPadding > 0)
			{
				/* Pad this file's end so the next file starts at a 256-byte boundary. */
				if ((fileIndex + 1) < inputFiles.size())
				{
					padBytes = ((size_t)doPadding - (partFile.buffer.size % (size_t)doPadding)) % (size_t)doPadding;
				}
			}

			printf("  input[%02zu]  start=$%08X  size=$%08X  pad=$%08X  \"%s\"\n",
				fileIndex + 1,
				(unsigned int)startOffset,
				(unsigned int)partFile.buffer.size,
				(unsigned int)padBytes,
				inputFiles[fileIndex].c_str());

			newSize = sourceFile.buffer.size + partFile.buffer.size + padBytes;
			newData = (byte *)realloc(sourceFile.buffer.data, newSize);
			if (newData == NULL)
			{
				printf("Error: Out of memory while concatenating input files.\n");
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

		if (!crunch(&sourceFile, &myBBBuffer))
		{
			free(sourceFile.buffer.data);
			free(outName);
			return -1;
		}

		if (!writeFile(&myBBBuffer, outName))
		{
			printf("Error (B-2): Write file \"%s\", aborting.\n", outName);
			free(sourceFile.buffer.data);
			free(myBBBuffer.data);
			free(outName);
			return -1;
		}

		printf("ByteBoozer summary:\n");
		printf("  files         : $%08X\n", (unsigned int)inputFiles.size());
		printf("  source bytes  : $%08X\n", (unsigned int)totalInputBytes);
		printf("  padding bytes : $%08X\n", (unsigned int)totalPaddingBytes);
		printf("  packed bytes  : $%08X\n", (unsigned int)myBBBuffer.size);
		if (sourceFile.buffer.size > 0)
		{
			double ratio = (double)myBBBuffer.size * 100.0 / (double)sourceFile.buffer.size;
			printf("  packed ratio: %.2f%%\n", ratio);
		}
		printf("  output: \"%s\"\n", outName);

		if (doValidate)
		{
			Buffer b3Buffer;

			b3Buffer.data = NULL;
			b3Buffer.size = 0;

			exitCode = decrunchToMemory(&myBBBuffer, &b3Buffer);
			if (exitCode == 0)
			{
				exitCode = validateBuffers(&sourceFile.buffer, &b3Buffer);
			}

			free(b3Buffer.data);
		}

		free(sourceFile.buffer.data);
		free(myBBBuffer.data);
		free(outName);

		return exitCode;
	}
}
