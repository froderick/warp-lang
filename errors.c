#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <wchar.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>

#include "errors.h"

const unsigned int DESCRIPTION_SIZE = 1024;

typedef struct InternalError {
  bool initialized;
  const char* system;
  const char* id;
  wchar_t description[DESCRIPTION_SIZE];
} InternalError;

const unsigned int QUEUE_SIZE = 16;

_Thread_local InternalError queue[QUEUE_SIZE];
_Thread_local unsigned int head = 0;
_Thread_local unsigned int depth = 0;

void reportError(const char* system, const char* id, wchar_t* description) {

  InternalError e;
  e.initialized = true;
  e.system = system;
  e.id = id;
  wcsncpy(e.description, description, DESCRIPTION_SIZE - 1);

  if (!queue[head].initialized) { // initial case
    head = 0;
    depth = 1;
  }
  else if (head + 1 == QUEUE_SIZE) { // queue overflow
    head = 0;
    depth = QUEUE_SIZE;
  }
  else { // subsequent case
    head = head + 1;
    depth = depth + 1;
  }

  queue[head] = e;
}

void reportErrnoError(const char* system, const char* id) {
  wchar_t buf[100];
  char *errorString = strerror(errno);
  mbstowcs(buf, errorString, 100);
  reportError(system, id, buf);
}

unsigned int errorDepth() {
  return depth;
}

Error getError(unsigned int errorDepth) {

  Error error;

  if (errorDepth > depth) {
    return error;
  }

  unsigned int idx = head;
  if (errorDepth > 0) {
    for (int i = 0; i < errorDepth; i++) {
      if (idx == 0) {
        idx = QUEUE_SIZE - 1;
      } else {
        idx = head - 1;
      }
    }
  }

  InternalError e = queue[idx];
  error.system = e.system;
  error.id = e.id;
  error.description = e.description;
  return error;
}

Error getLastError() {
  return getError(0);
}

void clearErrors() {
  for (int i=0; i<QUEUE_SIZE; i++) {
    queue[i].initialized = false;
    head = 0;
    depth = 0;
  }
}

void printErrors() {
  unsigned int idx = head;
  for (int i=0; i<depth; i++) {

    InternalError e = queue[idx];
    printf("[%i] %s/%s: %ls \n", i, e.system, e.id, e.description);

    if (idx == 0) {
      idx = QUEUE_SIZE - 1;
    }
    else {
      idx = head - 1;
    }
  }
}







