#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <wchar.h>
#include <stdbool.h>

void reportError(const char* system, const char* id, wchar_t* description);

typedef struct Error {
  const char* system;
  const char* id;
  wchar_t* description;
} Error;

unsigned int errorDepth();
Error getError(unsigned int errorDepth);
Error getLastError();
void clearErrors();
void printErrors();




