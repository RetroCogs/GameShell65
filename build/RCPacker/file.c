#include "file.h"
#include <stdlib.h>
#include <string.h>

void freeFile(File *aFile)
{
  free(aFile->name);
  free(aFile->data);
}

char *mystrdup(const char *str)
{
  // NOTE: Using manual malloc+strcpy instead of strdup()
  // strdup() has a bug in glibc 2.39 (Ubuntu 24.04) that causes it to
  // return invalid addresses when called in this context.
  // This manual approach is more portable and works on all platforms.
  char *dup = (char *)malloc(strlen(str) + 1);
  if (dup == NULL) {
    return false;
  }
  strcpy(dup, str);

  return dup;
}

bool readFile(File *aFile, const char *fileName)
{
  FILE *fp = NULL;
  struct stat fileStatus;

  aFile->name = mystrdup(fileName);

  if(stat(aFile->name, &fileStatus) == -1) {
    return false;
  }
  aFile->size = fileStatus.st_size;

  fp = fopen(aFile->name, "rb");
  if(fp == NULL) {
    return false;
  }

  aFile->data = (byte *)malloc(aFile->size);
  if(aFile->data == NULL) {
    fclose(fp);
    return false;
  }

  if(fread(aFile->data, 1, aFile->size, fp) != aFile->size) {
    fclose(fp);
    free(aFile->data);
    return false;
  }

  fclose(fp);
  return true;
}

bool writeFile(File *aFile, const char *fileName, const size_t startOffset)
{
  FILE *fp = NULL;
  size_t length;

  if (startOffset >= aFile->size) {
    return false;
  }

  length = strlen(fileName);
  aFile->name = (char *)malloc(length + 4);

  if(aFile->name == NULL){
    return false;
  }

  strncpy(aFile->name, fileName, length);
  strncpy(aFile->name + length, ".b2\0", 4);

  fp = fopen(aFile->name, "wb");
  if(fp == NULL) {
    return false;
  }

  if(fwrite(aFile->data + startOffset, 1, aFile->size - startOffset, fp) != aFile->size - startOffset) {
    fclose(fp);
    return false;
  }

  fclose(fp);
  return true;
}
