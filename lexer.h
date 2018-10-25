#include <stdio.h>
#include <wchar.h>
#include <stdbool.h>

typedef enum TokenType {
  // invalid token
  T_NONE,
  // static tokens
  T_OPAREN,
  T_CPAREN,
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
  wchar_t *text;
  unsigned long position;
  unsigned long length;
  bool textAllocated; // if indicates whether or not the text string is on the heap
} Token;

void tokenFree(Token *t);

typedef struct LexerState *LexerState_t;

bool tryLexerStateMake(LexerState_t *ptr);

void lexerStateFree(LexerState_t s);

// TODO: these docs are out of date
/**
 *
 * Attempts to read a token from the supplied stream. If it is successful, it
 * returns a pointer to the newly allocated token struct. It is the caller's
 * job to free the token memory when it is no longer used. 
 *
 * If `EOF` is encountered, the `err` pointer will be set to `ERR_EOF`.
 *
 * If `EOF` is encountered unexpectedly (only a partial token has been read),
 * the `err` pointer will be set to `ERR_UNEXPECTED_EOF`.
 *
 * All other errors will result in setting the `err` pointer to ERR_IO.
 */
bool tryTokenRead(FILE *stream, LexerState_t s, Token **token);

const char* tokenName(TokenType type);

typedef struct Tokens {
  Token **data;
  unsigned long used;
  unsigned long size;
} Tokens;

bool tryTokensRead(FILE *stream, Tokens **tokens);
void tokensFree(Tokens *l);
