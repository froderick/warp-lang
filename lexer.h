#ifndef WARP_LANG_LEXER_H
#define WARP_LANG_LEXER_H

#include <stdio.h>
#include <wchar.h>
#include <stdint.h>
#include <stdbool.h>
#include "errors.h"
#include "utils.h"
#include "source.h"

typedef enum TokenType {
  T_NONE,
  T_OPAREN,
  T_CPAREN,
  T_NUMBER,
  T_STRING,
  T_SYMBOL,
  T_KEYWORD,
  T_QUOTE,
  T_TRUE,
  T_FALSE,
  T_NIL
} TokenType;

typedef struct Token {
  TokenType type;
  const char* typeName;
  SourceLocation source;
  wchar_t text[];
} Token;

void tokenFree(Token *t);

typedef struct TokenStream *TokenStream_t;

RetVal tryStreamMake(InputStream_t source, TokenStream_t *s, Error *error);
RetVal tryStreamFree(TokenStream_t s, Error *error);

// stream operations
RetVal tryStreamNext(TokenStream_t s, Token **token, Error *error);
RetVal tryStreamPeek(TokenStream_t s, Token **token, Error *error);
void streamDropPeeked(TokenStream_t s);

#endif //WARP_LANG_LEXER_H
