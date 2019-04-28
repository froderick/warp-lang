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
#include "pool.h"

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

typedef struct MapElement {
  struct Expr *key;
  struct Expr *value;
  struct MapElement *next;
} MapElement;

typedef struct ExprMap {
  uint64_t length;
  MapElement *head;
  MapElement *tail;
} ExprMap;

typedef enum ExprType {
  N_NONE,
  N_STRING,
  N_NUMBER,
  N_SYMBOL,
  N_KEYWORD,
  N_BOOLEAN,
  N_NIL,
  N_LIST,
  N_MAP
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
    ExprMap map;
  };
  SourceLocation source;
} Expr;

void exprInitContents(Expr *expr);

RetVal tryExprRead(Pool_t pool, TokenStream_t stream, Expr **expr, Error *error);

RetVal tryExprDeepCopy(Pool_t pool, Expr *from, Expr **ptr, Error *error);

RetVal tryExprPrnBufConf(Expr *expr, StringBuffer_t b, bool readable, Error *error);
RetVal tryExprPrnBuf(Expr *expr, StringBuffer_t b, Error *error);
RetVal tryExprPrnStr(Pool_t pool, Expr *expr, wchar_t **ptr, Error *error);
RetVal tryExprPrn(Pool_t pool, Expr* expr, Error *error);

void listInitContents(ExprList *list);
RetVal tryListAppend(Pool_t pool, ExprList *list, Expr *expr, Error *error);

void mapInitContents(ExprMap *map);
RetVal tryMapPut(Pool_t pool, ExprMap *map, Expr *key, Expr *value, Error *error);

#endif //WARP_LANG_READER_H
