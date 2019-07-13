#ifndef WARP_LANG_LEXER_H
#define WARP_LANG_LEXER_H

#include <stdio.h>
#include <wchar.h>
#include <stdint.h>
#include <stdbool.h>
#include "errors.h"
#include "utils.h"
#include "source.h"
#include "pool.h"

typedef enum TokenType {
  T_NONE,
  T_OPAREN,
  T_CPAREN,
  T_NUMBER,
  T_STRING,
  T_SYMBOL,
  T_KEYWORD,
  T_QUOTE,
  T_SYNTAX_QUOTE,
  T_UNQUOTE,
  T_SPLICING_UNQUOTE,
  T_TRUE,
  T_FALSE,
  T_NIL,
  T_COMMENT,
  T_OBRACKET,
  T_CBRACKET,
  T_OVEC,
  T_CVEC,
} TokenType;

typedef struct Token {
  TokenType type;
  const char* typeName;
  SourceLocation source;
  wchar_t text[];
} Token;

typedef struct TokenStream *TokenStream_t;

RetVal tryStreamMake(Pool_t pool, InputStream_t source, TokenStream_t *s, Error *error);

// stream operations
RetVal tryStreamNext(Pool_t pool, TokenStream_t s, Token **token, Error *error);
RetVal tryStreamPeek(Pool_t pool, TokenStream_t s, Token **token, Error *error);
void streamDropPeeked(TokenStream_t s);

#endif //WARP_LANG_LEXER_H
