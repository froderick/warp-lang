#include <stdio.h>
#include <wchar.h>
#include "errors.h"

typedef enum TokenType {
  // invalid token
  T_NONE,
  // static tokens
//  T_OVEC,
//  T_CVEC,
//  T_OBRACKET,
//  T_CBRACKET,
  // value tokens
  T_OPAREN,
  T_CPAREN,
  T_NUMBER,
  T_STRING,
  T_SYMBOL,
  T_KEYWORD
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

typedef struct ExprSpecial {
  Token *token;
  wchar_t *value;
} ExprSpecial;

typedef struct ExprBoolean {
  Token *token;
  bool value;
} ExprBoolean;

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
  N_NUMBER,
  N_STRING,
  N_SYMBOL,
  N_KEYWORD,
  N_QUOTE,
  N_BOOLEAN,
  N_NIL,
  N_LIST
//  N_ATOM,
//  N_VECTOR,
//  N_MAP,
//  N_SET,
} ExprType;

typedef struct Expr {
  ExprType type;
  union {
    ExprString string;
    ExprNumber number;
    ExprSymbol symbol;
    ExprKeyword keyword;
    ExprSpecial special;
    ExprBoolean boolean;
    ExprList list;
  };
} Expr;

RetVal tryReadExpr(TokenStream_t stream, Expr **expr, Error *error);

