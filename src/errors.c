#include <wchar.h>
#include <string.h>
#include <errno.h>
#include <libgen.h>
#include<stdio.h>
#include <inttypes.h>
#include "errors.h"

void errorInitContents(Error *error) {
  error->lineNumber = 0;
  error->functionName = NULL;
  error->fileName = NULL;
  error->type = E_NONE;
}

void printError(Error *error) {
  char* fileName = basename((char *) error->fileName);
  printf("(debug) error at %s(%s:%" PRIu64 "): %ls\n", error->functionName, fileName, error->lineNumber, error->message);
}

/*
 * Error factories
 */

RetVal memoryError(Error *error, char *desc) {

  error->type = E_MEMORY;

  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to %s\n", desc);

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

RetVal ioError(Error *error, char *desc) {

  error->type = E_IO;
  error->lexer.position = 0;
  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to %s ->  '%s'\n", desc, strerror(errno));

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

RetVal internalError(Error *error, char *desc) {

  error->type = E_INTERNAL;
  swprintf(error->message, ERROR_MSG_LENGTH, L"encountered internal failure, probably a bug: %s\n", desc, strerror(errno));

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

RetVal tokenizationError(Error *error, unsigned long position, char *desc) {

  error->type = E_LEXER;
  error->lexer.position = position;
  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to tokenize stream -> %s\n", desc);

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

RetVal syntaxError(Error *error, unsigned long position, char *desc) {

  error->type = E_SYNTAX;
  error->lexer.position = position;
  swprintf(error->message, ERROR_MSG_LENGTH, L"invalid syntax encountered -> %s\n", desc);

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

RetVal runtimeError(Error *error, char *desc) {

  error->type = E_RUNTIME;
  swprintf(error->message, ERROR_MSG_LENGTH, L"vm failure encountered -> %s\n", desc);

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

RetVal compilerError(Error *error, char *desc) {

  error->type = E_RUNTIME;
  swprintf(error->message, ERROR_MSG_LENGTH, L"compiler error encountered -> %s\n", desc);

  if (DEBUG) { printError(error); }
  return R_ERROR;
}

