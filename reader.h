#ifndef WARP_LANG_READER_H
#define WARP_LANG_READER_H

#include <stdio.h>
#include <wchar.h>
#include <stdint.h>
#include <stdbool.h>
#include "errors.h"
#include "utils.h"
#include "source.h"
#include "lexer.h"

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
void exprFreeContents(Expr *expr);
void exprFree(Expr *expr);

RetVal tryExprDeepCopy(Expr *from, Expr **ptr, Error *error);

RetVal tryExprPrnStr(Expr *expr, wchar_t **ptr, Error *error);
RetVal tryExprPrn(Expr* expr, Error *error);

void listInitContents(ExprList *list);
RetVal tryListAppend(ExprList *list, Expr *expr, Error *error);

#endif //WARP_LANG_READER_H
