#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

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
        for (int i = 1; i < argc; i++)
        {
            printf("argv[%d] = %s\n", i, argv[i]);
        }
    }

    return 0;
}
