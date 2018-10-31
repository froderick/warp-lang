#include <stdio.h>
#include <wchar.h>

#define LEX_SUCCESS  0
#define LEX_EOF      1
#define LEX_ERROR    2

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

typedef struct TokenStream *TokenStream_t;

int tryStreamMakeFile(char *filename, TokenStream_t *s);
int tryStreamMake(FILE *file, TokenStream_t *s);

int tryStreamNext(TokenStream_t s, Token **ptr);
int tryStreamPeek(TokenStream_t s, Token **ptr);
void tokenFree(Token *t);
int tryStreamFree(TokenStream_t s);


