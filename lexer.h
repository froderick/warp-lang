#include <stdio.h>
#include <wchar.h>
#include "errors.h"

typedef enum TokenType {
  // invalid token
  T_NONE,
  // static tokens
  T_OPAREN,
  T_CPAREN,
  T_OVEC,
  T_CVEC,
  T_OBRACKET,
  T_CBRACKET,
  T_TRUE,
  T_FALSE,
  T_NIL,
  T_QUOTE,
  // value tokens
  T_NUMBER,
  T_STRING,
  T_SYMBOL,
  T_KEYWORD
} TokenType;

typedef struct Token {
  TokenType type;
  const char* typeName;
  unsigned long position;
  unsigned long length;
  wchar_t text[];
} Token;

typedef struct StreamSource *StreamSource_t;

RetVal trySourceMake(
    void *state,
    RetVal (*readChar)(void *state, wchar_t *ch, Error *error),
    RetVal (*unreadChar)(void * state, wchar_t ch, Error *error),
    RetVal (*freeState)(void *state, Error *error),
    StreamSource_t *s,
    Error *error
);
RetVal trySourceMakeFilename(char *filename, StreamSource_t *s, Error *error);
RetVal trySourceMakeFile(FILE *file, StreamSource_t *s, Error *error);
RetVal trySourceMakeString(wchar_t* text, uint64_t length, StreamSource_t *s, Error *error);
RetVal trySourceFree(StreamSource_t s, Error *error);

typedef struct TokenStream *TokenStream_t;

RetVal tryStreamMake(StreamSource_t source, TokenStream_t *s, Error *error);
RetVal tryStreamNext(TokenStream_t s, Token **token, Error *error);
RetVal tryStreamPeek(TokenStream_t s, Token **token, Error *error);
void tokenFree(Token *t);
RetVal tryStreamFree(TokenStream_t s, Error *error);


