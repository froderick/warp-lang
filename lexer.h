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

typedef enum LexerErrorType {
  LE_MEMORY,
  LE_IO,
  LE_TOKENIZATION
} LexerErrorType;

typedef struct LexerError {
  LexerErrorType type;
  /*
   * The lexer's position in the stream at the time the error occurred.
   * This is only non-zero if #type == #LE_TOKENIZATION.
   */
  unsigned long position;
  wchar_t message[1024];
} LexerError;

typedef struct TokenStream *TokenStream_t;

int tryStreamMakeFile(char *filename, TokenStream_t *s, LexerError *error);
int tryStreamMake(FILE *file, TokenStream_t *s, LexerError *error);

int tryStreamNext(TokenStream_t s, Token **token, LexerError *error);
int tryStreamPeek(TokenStream_t s, Token **token, LexerError *error);
void tokenFree(Token *t);
int tryStreamFree(TokenStream_t s, LexerError *error);


