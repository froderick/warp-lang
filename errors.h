#include <stdint.h>

typedef enum ErrorType {
  E_MEMORY,
  E_IO,
  E_LEXER
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
RetVal tokenizationError(Error *error, unsigned long position, char *desc);
