#ifndef WARP_LANG_ERRORS_H
#define WARP_LANG_ERRORS_H

#include <stdint.h>
#include <wchar.h>

typedef enum ErrorType {
  E_MEMORY,
  E_IO,
  E_INTERNAL,
  E_LEXER,
  E_SYNTAX,
  E_RUNTIME,
} ErrorType;

typedef struct LexerError {
  /*
   * The lexer's position in the stream at the time the error occurred.
   * This is only non-zero if #type == #E_LEXER.
   */
  unsigned long position;

} LexerError;

#define ERROR_MSG_LENGTH 1023

typedef struct Error {
  ErrorType type;
  union {
    LexerError lexer;
  };
  wchar_t message[ERROR_MSG_LENGTH + 1];
  const char *fileName;
  uint64_t lineNumber;
  const char *functionName;
} Error;


/*
 * These are the core return codes:
 * - #R_SUCCESS means whatever was attempted succeeded.
 * - #R_ERROR means that the error struct has been populated
 *   with more information.
 *
 * The remainder of the codes in here are situation-specific. They are
 * represented here so I can keep track of them.
 *
 * Yes, this leaks implementation details of the application here. I'm open to
 * suggestions.
 */
typedef enum RetVal {

  R_SUCCESS,
  R_ERROR,

  /*
   * This code is returned by the lexer to indicate that no next token is
   * available to be read. It isn't an error unless you can't continue
   * without another token.
   */
  R_EOF
} RetVal;

/*
 * Error factories
 */

RetVal memoryError(Error *error, char *desc);
RetVal ioError(Error *error, char *desc);
RetVal internalError(Error *error, char *desc);
RetVal tokenizationError(Error *error, unsigned long position, char *desc);
RetVal syntaxError(Error *error, unsigned long position, char *desc);
RetVal runtimeError(Error *error, char *desc);

#define throwMemoryError(error, str, ...) {\
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  ret = memoryError(error, msg); \
  error->fileName = __FILE__; \
  error->lineNumber = __LINE__; \
  error->functionName = __func__; \
  goto failure; \
}

#define throwIOError(error, str, ...) {\
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  ret = ioError(error, msg); \
  error->fileName = __FILE__; \
  error->lineNumber = __LINE__; \
  error->functionName = __func__; \
  goto failure; \
}

#define throwInternalError(error, str, ...) {\
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  ret = internalError(error, msg); \
  error->fileName = __FILE__; \
  error->lineNumber = __LINE__; \
  error->functionName = __func__; \
  goto failure; \
}

#define throwTokenizationError(error, pos, str, ...) {\
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  ret = tokenizationError(error, pos, msg); \
  error->fileName = __FILE__; \
  error->lineNumber = __LINE__; \
  error->functionName = __func__; \
  goto failure; \
}

#define throwSyntaxError(error, pos, str, ...) {\
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  ret = syntaxError(error, pos, msg); \
  error->fileName = __FILE__; \
  error->lineNumber = __LINE__; \
  error->functionName = __func__; \
  goto failure; \
}

#define throwRuntimeError(error, str, ...) {\
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  ret = runtimeError(error, msg); \
  error->fileName = __FILE__; \
  error->lineNumber = __LINE__; \
  error->functionName = __func__; \
  goto failure; \
}

#define throws(f) {\
  ret = f;\
  if (ret != R_SUCCESS) {\
    goto failure;\
  }\
}

#define tryMalloc(var, size, desc) {\
  var = malloc(size);\
  if (var == NULL) {\
    ret = memoryError(error, desc);\
    error->fileName = __FILE__; \
    error->lineNumber = __LINE__; \
    error->functionName = __func__; \
    goto failure; \
  }\
}

#endif //WARP_LANG_ERRORS_H

