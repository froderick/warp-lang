#ifndef WARP_LANG_READER_H
#define WARP_LANG_READER_H

#include <stdio.h>
#include <wchar.h>
#include <stdint.h>
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


typedef struct SourceLocation {
  bool isSet;
  uint64_t position;
  uint64_t lineNumber;
  uint64_t colNumber;
  uint64_t length;
} SourceLocation;

typedef struct Token {
  TokenType type;
  const char* typeName;
  SourceLocation source;
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
  wchar_t *value;
  uint64_t length;
} ExprString;

typedef struct ExprNumber {
  uint64_t value;
} ExprNumber;

typedef struct ExprSymbol {
  wchar_t *value;
  uint64_t length;
} ExprSymbol;

typedef struct ExprKeyword {
  wchar_t *value;
  uint64_t length;
} ExprKeyword;

typedef struct ExprBoolean {
  bool value;
} ExprBoolean;

typedef struct ListElement {
  struct Expr *expr;
  struct ListElement *next;
} ListElement;

typedef struct ExprList {
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
    ExprList list;
  };
  SourceLocation source;
} Expr;

RetVal tryExprRead(TokenStream_t stream, Expr **expr, Error *error);
void exprFree(Expr *expr);

RetVal tryExprDeepCopy(Expr *from, Expr **ptr, Error *error);

#endif //WARP_LANG_READER_H
