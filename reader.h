#include <stdio.h>
#include <wchar.h>
#include <stdbool.h>
#include "errors.h"

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
  unsigned long position;
  unsigned long length;
  unsigned long lineNumber;
  unsigned long colNumber;
  // TODO: track rows/cols within reader, include start/end row/cols within each token
  wchar_t text[];
} Token;

void tokenFree(Token *t);

typedef struct StreamSource *StreamSource_t;

RetVal trySourceMake(
    void *state,
    RetVal (*readChar)(void *state, wchar_t *ch, Error *error),
    RetVal (*unreadChar)(void * state, wchar_t ch, Error *error),
    RetVal (*freeState)(void *state, Error *error),
    StreamSource_t *s,
    Error *error
);
RetVal trySourceFree(StreamSource_t s, Error *error);

// source factories
RetVal trySourceMakeFile(FILE *file, StreamSource_t *s, Error *error);
RetVal trySourceMakeString(wchar_t* text, uint64_t length, StreamSource_t *s, Error *error);

typedef struct TokenStream *TokenStream_t;

RetVal tryStreamMake(StreamSource_t source, TokenStream_t *s, Error *error);
RetVal tryStreamFree(TokenStream_t s, Error *error);

// stream operations
RetVal tryStreamNext(TokenStream_t s, Token **token, Error *error);
RetVal tryStreamPeek(TokenStream_t s, Token **token, Error *error);

struct Expr;

typedef struct ExprString {
  Token *token;
  uint64_t length;
  wchar_t *value;
} ExprString;

typedef struct ExprNumber {
  Token *token;
  uint64_t value;
} ExprNumber;

typedef struct ExprSymbol {
  Token *token;
  uint64_t length;
  wchar_t *value;
} ExprSymbol;

typedef struct ExprKeyword {
  Token *token;
  uint64_t length;
  wchar_t *value;
} ExprKeyword;

typedef struct ExprBoolean {
  Token *token;
  bool value;
} ExprBoolean;

typedef struct ExprNil {
  Token *token;
} ExprNil;

typedef struct ListElement {
  struct Expr *expr;
  struct ListElement *next;
} ListElement;

typedef struct ExprList {
  Token* oParen;
  Token* cParen;
  uint64_t length;
  ListElement *head;
  ListElement *tail;
} ExprList;

typedef enum ExprType {
  N_NONE,
  N_STRING,
  N_NUMBER,
  N_SYMBOL,
  N_KEYWORD,
  N_BOOLEAN,
  N_NIL,
  N_LIST
} ExprType;

typedef struct Expr {
  ExprType type;
  union {
    ExprString string;
    ExprNumber number;
    ExprSymbol symbol;
    ExprKeyword keyword;
    ExprBoolean boolean;
    ExprNil nil;
    ExprList list;
  };
} Expr;

RetVal tryExprRead(TokenStream_t stream, Expr **expr, Error *error);
void exprFree(Expr *expr);

