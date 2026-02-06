#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#include "file.h"
#include "cruncher.h"

int main(int argc, char * argv[])
{
    printf("argc = %d\n", argc);

    if (argc == 1)
    {
        printf("RCPacker [packer for Mega65] usage:\n");
        printf("\tblah blah\n");
        printf("\tblah blah\n");
        printf("\tblah blah\n");
    }
    else
    {
        File myFile;
        File myBBFile;
        char* fileName;
        bool isExecutable = false;
        bool isRelocated = false;
          bool clipStartAddress = false;
        uint address = 0;

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
        }

        printf("ByteBoozer: \"%s\" -> \"%s\"\n", myFile.name, myBBFile.name);

        freeFile(&myFile);
        freeFile(&myBBFile);

    }

    return 0;
}
