#include <wchar.h>
#include <string.h>
#include <errno.h>
#include "errors.h"

/*
 * Error factories
 */

RetVal memoryError(Error *error, char *desc) {

  error->type = E_MEMORY;

  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to %s\n", desc);

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return R_ERROR;
}

RetVal ioError(Error *error, char *desc) {

  error->type = E_IO;
  error->lexer.position = 0;
  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to %s ->  '%s'\n", desc, strerror(errno));

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return R_ERROR;
}

RetVal tokenizationError(Error *error, unsigned long position, char *desc) {

  error->type = E_LEXER;
  error->lexer.position = position;
  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to tokenize stream -> %s\n", desc);

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return R_ERROR;
}

RetVal syntaxError(Error *error, unsigned long position, char *desc) {

  error->type = E_SYNTAX;
  error->lexer.position = position;
  swprintf(error->message, ERROR_MSG_LENGTH, L"failed to parse token stream -> %s\n", desc);

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return R_ERROR;
}
