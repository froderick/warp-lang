#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>
#include <errno.h>
#include <inttypes.h>
#include <assert.h>

#include "utils.h"
#include "lexer.h"
#include "reader.h"
#include "pool.h"

/*
 * Here is the basic AST implementation.
 */

RetVal tryStringMake(Pool_t pool, wchar_t *input, uint64_t length, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *text;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  throws(tryCopyText(pool, input, &text, length, error));

  expr->type = N_STRING;
  expr->string.length = length;
  expr->string.value = text;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryStringRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_STRING) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_STRING: %u", token->type);
  }

  if (token->source.length < 2) {
    throwSyntaxError(error, token->source.position, "Token should start and end with '\"'");
  }

  // trim quotes
  wchar_t *text = token->text + 1;
  uint64_t len = token->source.length - 2;

  Expr *expr;
  throws(tryStringMake(pool, text, len, &expr, error));

  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryNumberMake(Pool_t pool, uint64_t value, Expr **ptr, Error *error) {
  RetVal ret;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  expr->type = N_NUMBER;
  expr->number.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryNumberRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_NUMBER) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_NUMBER: %u", token->type);
  }

  errno = 0;
  uint64_t value = wcstoull(token->text, NULL, 0);
  if (errno == EINVAL) {
    throwSyntaxError(error, token->source.position, "Token text does not represent a valid number: '%ls'", token->text);
  }
  if (errno == ERANGE) {
    throwSyntaxError(error, token->source.position, "Cannot represent a number literal larger than 64 bits unsigned");
  }

  Expr *expr;
  throws(tryNumberMake(pool, value, &expr, error));

  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

/*
 * This is for dynamically creating symbols, for instance to handle the reader
 * macros where certain tokens (like '`') expand into special forms.
 */
RetVal trySymbolMake(Pool_t pool, wchar_t *name, uint64_t len, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *value;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  throws(tryCopyText(pool, name, &value, len, error));

  expr->type = N_SYMBOL;
  expr->symbol.value = value;
  expr->symbol.length = len;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal trySymbolRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_SYMBOL) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_SYMBOL: %u", token->type);
  }

  Expr *expr;
  throws(trySymbolMake(pool, token->text, token->source.length, &expr, error));

  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryKeywordMake(Pool_t pool, wchar_t *name, uint64_t len, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *text;

  throws(tryCopyText(pool, name, &text, len, error));

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  expr->type = N_KEYWORD;
  expr->keyword.length = len;
  expr->keyword.value = text;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryKeywordRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_KEYWORD) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_KEYWORD: %u", token->type);
  }

  uint64_t len = token->source.length - 1;
  wchar_t *text = token->text + 1;

  Expr *expr;
  throws(tryKeywordMake(pool, text, len, &expr, error));

  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryBooleanMake(Pool_t pool, bool value, Expr **ptr, Error *error) {
  RetVal ret;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  expr->type = N_BOOLEAN;
  expr->boolean.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryBooleanRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_TRUE && token->type != T_FALSE) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_TRUE or T_FALSE");
  }

  bool value = token->type == T_TRUE;

  Expr *expr;
  throws(tryBooleanMake(pool, value, &expr, error));

  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryNilMake(Pool_t pool, Expr **ptr, Error *error) {
  RetVal ret;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  expr->type = N_NIL;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryNilRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_NIL) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_NIL: %u", token->type);
  }

  Expr *expr;
  throws(tryNilMake(pool, &expr, error));

  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

// Lists

RetVal tryExprRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error);

// Assume the opening paren has aready been read.
// Allocate a list, continue to read expressions and add them to it until a
// closed-paren is found.

RetVal tryListMake(Pool_t pool, Expr **ptr, Error *error) {
  RetVal ret;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  expr->type = N_LIST;
  expr->list.length = 0;

  // valid for zero length list
  expr->list.head = NULL;
  expr->list.tail = NULL;

  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

void listInitContents(ExprList *list) {
  list->length = 0;
  list->head = NULL;
  list->tail = NULL;
}

RetVal tryListRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Token *oParen = NULL, *cParen = NULL;
  Expr *expr = NULL;
  Expr *subexpr = NULL;

  // convenience

  throws(tryListMake(pool, &expr, error));

  throws(tryStreamNext(pool, stream, &oParen, error));

  if (oParen->type != T_OPAREN) {
    throwSyntaxError(error, oParen->source.position, "List must begin with ')': %u", oParen->type);
  }

  while (true) {

    ret = tryStreamPeek(pool, stream, &cParen, error);
    if (ret != R_SUCCESS) {
      if (ret == R_EOF) { // eof too soon
        throwSyntaxError(error, oParen->source.position, "Encountered EOF, was expecting ')'");
      }
      goto failure;
    }

    if (cParen->type == T_CPAREN) { // found our closed paren

      streamDropPeeked(stream); // consume cParen now that nothing else can fail

      expr->source = oParen->source;
      expr->source.length = cParen->source.position - oParen->source.position;

      *ptr = expr;
      break;
    }
    else { // read a new expression and add it to the list
      cParen = NULL;
      throws(tryExprRead(pool, stream, &subexpr, error));
      throws(tryListAppend(pool, &expr->list, subexpr, error));
      subexpr = NULL; // subexpr is part of list now
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryListAppend(Pool_t pool, ExprList *list, Expr *expr, Error *error) {
  RetVal ret;

  ListElement *elem;
  tryPalloc(pool, elem, sizeof(ListElement), "ExprList");

  elem->expr = expr;
  elem->next = NULL;

  if (list->head == NULL) { // no elements
    list->head = elem;
    list->tail = elem;
  }
  else if (list->head == list->tail) { // one element
    list->head->next = elem;
    list->tail = elem;
  }
  else { // more than one element
    list->tail->next = elem;
    list->tail = elem;
  }

  list->length = list->length + 1;

  return R_SUCCESS;

  failure:
    return ret;
}

// maps

void mapInitContents(ExprMap *map) {
  map->length = 0;
  map->head = NULL;
  map->tail = NULL;
}

void mapElementInitContents(MapElement *e) {
  e->key = NULL;
  e->value = NULL;
  e->next = NULL;
}

RetVal tryMapMake(Pool_t pool, Expr **ptr, Error *error) {
  RetVal ret;

  Expr *expr;
  tryPalloc(pool, expr, sizeof(Expr), "Expr");

  expr->type = N_MAP;
  expr->source.isSet = false;

  // valid for zero length map
  mapInitContents(&expr->map);

  *ptr = expr;
  return R_SUCCESS;

  failure:
  return ret;
}

bool exprEquals(Expr *a, Expr *b) {

  if (a->type != b->type) {
    return false;
  }
  else {
    switch (a->type) {

      case N_NIL:
        return true;

      case N_NUMBER:
        return a->number.value == b->number.value;

      case N_BOOLEAN:
        return a->boolean.value == b->boolean.value;

      case N_STRING:
        return wcscmp(a->string.value, b->string.value) == 0;

      case N_SYMBOL:
        return wcscmp(a->symbol.value, b->symbol.value) == 0;

      case N_KEYWORD:
        return wcscmp(a->keyword.value, b->keyword.value) == 0;

      case N_LIST:
        if (a->list.length != b->list.length) {
          return false;
        }
        else {
          ListElement *aseq = a->list.head;
          ListElement *bseq = b->list.head;

          while (aseq != NULL) {
            if (!exprEquals(aseq->expr, bseq->expr)) {
              return false;
            }
            aseq = aseq->next;
            bseq = bseq->next;
          }
          return true;
        }

      case N_MAP:
        // make sure there are the same numbers of distinct key entries in a and b
        if (a->map.length != b->map.length) {
          return false;
        }
        else {

          // iterate over the keys in a
          MapElement *amap = a->map.head;
          while (amap != NULL) {

            // make sure all the keys and values from a are in b
            MapElement *found = NULL;
            {
              MapElement *bmap = b->map.head;
              while (bmap != NULL) {
                if (exprEquals(amap->key, bmap->key)) {
                  found = bmap;
                }
                bmap = bmap->next;
              }
            }

            if (found == NULL) {
              return false;
            }

            if (!exprEquals(amap->value, found->value)) {
              return false;
            }

            amap = amap->next;
          }

          return true;
        }

      default:
        explode("unhandled expression type: %u", a->type);
    }
  }
}


RetVal tryMapPut(Pool_t pool, ExprMap *map, Expr *key, Expr *value, Error *error) {
  RetVal ret;

  // make sure all the keys and values from a are in b
  MapElement *found = NULL;
  {
    MapElement *elem = map->head;
    while (elem != NULL) {
      if (exprEquals(key, elem->key)) {
        found = elem;
      }
      elem = elem->next;
    }
  }

  if (found != NULL) {
    found->value = value;
  }
  else {
    MapElement *elem;
    tryPalloc(pool, elem, sizeof(MapElement), "MapElement");

    mapElementInitContents(elem);

    elem->key = key;
    elem->value = value;

    if (map->head == NULL) { // no elements
      map->head = elem;
      map->tail = elem;
    }
    else if (map->head == map->tail) { // one element
      map->head->next = elem;
      map->tail = elem;
    }
    else { // more than one element
      map->tail->next = elem;
      map->tail = elem;
    }

    map->length = map->length + 1;
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryMapRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Token *oParen = NULL, *cParen = NULL;
  Expr *expr = NULL;

  // convenience

  throws(tryMapMake(pool, &expr, error));

  throws(tryStreamNext(pool, stream, &oParen, error));

  if (oParen->type != T_OBRACKET) {
    throwSyntaxError(error, oParen->source.position, "Map must begin with '{': %u", oParen->type);
  }

  while (true) {

    ret = tryStreamPeek(pool, stream, &cParen, error);
    if (ret != R_SUCCESS) {
      if (ret == R_EOF) { // eof too soon
        throwSyntaxError(error, oParen->source.position, "Encountered EOF, was expecting '}'");
      }
      goto failure;
    }

    if (cParen->type == T_CBRACKET) { // found our closed paren

      streamDropPeeked(stream); // consume cParen now that nothing else can fail

      expr->source = oParen->source;
      expr->source.length = cParen->source.position - oParen->source.position;

      *ptr = expr;
      break;
    }
    else { // read a new entry and add it to the map
      Expr *key = NULL, *value = NULL;
      throws(tryExprRead(pool, stream, &key, error));
      throws(tryExprRead(pool, stream, &value, error));
      throws(tryMapPut(pool, &expr->map, key, value, error));
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryQuoteRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;

  Token *token;
  Expr *quote;
  Expr *subexpr;
  Expr *expr;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_QUOTE) {
    throwSyntaxError(error, token->source.position, "Quote must begin with ': %u", token->type);
  }

  throws(trySymbolMake(pool, L"quote", wcslen(L"quote"), &quote, error));
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  throws(tryExprRead(pool, stream, &subexpr, error));

  throws(tryListMake(pool, &expr, error));

  throws(tryListAppend(pool, &expr->list, quote, error));
  quote = NULL; // quote is now part of expr

  throws(tryListAppend(pool, &expr->list, subexpr, error));
  // subexpr is now part of expr

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryWrapperRead(Pool_t pool, TokenStream_t stream, wchar_t *symbolName, Expr **ptr, Error *error) {
  RetVal ret;

  Token *token;
  Expr *quote;
  Expr *subexpr;
  Expr *expr;

  throws(tryStreamNext(pool, stream, &token, error));

//  if (token->type != tokenType) {
//    throwSyntaxError(error, token->source.position, "%ls must begin with %lc: %u", symbolName, startsWith, token->type);
//  }

  throws(trySymbolMake(pool, symbolName, wcslen(symbolName), &quote, error));
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  throws(tryExprRead(pool, stream, &subexpr, error));

  throws(tryListMake(pool, &expr, error));

  throws(tryListAppend(pool, &expr->list, quote, error));
  quote = NULL; // quote is now part of expr

  throws(tryListAppend(pool, &expr->list, subexpr, error));
  // subexpr is now part of expr

  *ptr = expr;
  return R_SUCCESS;

  failure:
  return ret;
}

// read the first token off the stream
// if it is an open paren, parse a list
// if it is a symbol, create a symbol
// else, explode

RetVal tryExprRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {
  RetVal ret;

  Token *peek;

  throws(tryStreamPeek(pool, stream, &peek, error));

  // discard comments from the token stream for now
  // maybe someday we'll use them for autodoc purposes
  while (peek->type == T_COMMENT) {

    Token *discard;
    throws(tryStreamNext(pool, stream, &discard, error)); // discard the thing we peeked
    peek = NULL;

    throws(tryStreamPeek(pool, stream, &peek, error));
  }

  switch (peek->type) {

    // atoms
    case T_STRING:
      throws(tryStringRead(pool, stream, ptr, error));
      break;
    case T_NUMBER:
      throws(tryNumberRead(pool, stream, ptr, error));
      break;
    case T_SYMBOL:
      throws(trySymbolRead(pool, stream, ptr, error));
      break;
    case T_KEYWORD:
      throws(tryKeywordRead(pool, stream, ptr, error));
      break;
    case T_TRUE:
    case T_FALSE:
      throws(tryBooleanRead(pool, stream, ptr, error));
      break;
    case T_NIL:
      throws(tryNilRead(pool, stream, ptr, error));
      break;

    // lists
    case T_OPAREN:
      throws(tryListRead(pool, stream, ptr, error));
      break;

    // maps
    case T_OBRACKET:
      throws(tryMapRead(pool, stream, ptr, error));
      break;

    // reader macros
    case T_QUOTE:
      throws(tryQuoteRead(pool, stream, ptr, error));
      break;
    case T_SYNTAX_QUOTE:
      throws(tryWrapperRead(pool, stream, L"syntax-quote", ptr, error));
      break;
    case T_UNQUOTE:
      throws(tryWrapperRead(pool, stream, L"unquote", ptr, error));
      break;
    case T_SPLICING_UNQUOTE:
      throws(tryWrapperRead(pool, stream, L"splicing-unquote", ptr, error));
      break;

    default:
      throwSyntaxError(error, peek->source.position, "Unknown token type: %u", peek->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void exprInitContents(Expr *expr) {
  expr->type = N_NONE;
  sourceLocationInitContents(&expr->source);
}

RetVal tryExprDeepCopy(Pool_t pool, Expr *from, Expr **ptr, Error *error) {

  RetVal ret;

  // these get cleaned up on failure
  Expr *to;

  switch (from->type) {

    // atoms
    case N_STRING:
      throws(tryStringMake(pool, from->string.value, from->string.length, &to, error));
      break;
    case N_NUMBER:
      throws(tryNumberMake(pool, from->number.value, &to, error));
      break;
    case N_SYMBOL:
      throws(trySymbolMake(pool, from->symbol.value, from->symbol.length, &to, error));
      break;
    case N_KEYWORD:
      throws(tryKeywordMake(pool, from->keyword.value, from->keyword.length, &to, error));
      break;
    case N_BOOLEAN:
      throws(tryBooleanMake(pool, from->boolean.value, &to, error));
      break;
    case N_NIL:
      throws(tryNilMake(pool, &to, error));
      break;
    case N_LIST: {
      throws(tryListMake(pool, &to, error));

      ListElement *elem = from->list.head;
      while (elem != NULL) {

        Expr *listItem = NULL;
        throws(tryExprDeepCopy(pool, elem->expr, &listItem, error));
        throws(tryListAppend(pool, &to->list, listItem, error));

        elem = elem->next;
      }
      break;
    }
    case N_MAP: {
      throws(tryMapMake(pool, &to, error));

      MapElement *elem = from->map.head;
      while (elem != NULL) {

        Expr *k = NULL, *v = NULL;
        throws(tryExprDeepCopy(pool, elem->key, &k, error));
        throws(tryExprDeepCopy(pool, elem->value, &v, error));
        throws(tryMapPut(pool, &to->map, k, v, error));

        elem = elem->next;
      }
      break;
    }

    default:
      throwSyntaxError(error, from->source.position, "Unknown expr type '%i'", from->type);
  }

  to->source = from->source;
  *ptr = to;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryExprPrnBufConf(Expr *expr, StringBuffer_t b, bool readable, Error *error) {
  RetVal ret;

  switch (expr->type) {
    case N_NIL:
    throws(tryStringBufferAppendStr(b, L"nil", error));
      break;
    case N_NUMBER: {
      wchar_t text[256];
      swprintf(text, sizeof(text), L"%" PRIu64, expr->number.value);
      throws(tryStringBufferAppendStr(b, text, error));
      break;
    }
    case N_BOOLEAN:
      if (expr->boolean.value == 0) {
        throws(tryStringBufferAppendStr(b, L"false", error));
      }
      else {
        throws(tryStringBufferAppendStr(b, L"true", error));
      }
      break;
    case N_STRING: {
      if (readable) {
        throws(tryStringBufferAppendChar(b, L'"', error));
        throws(tryStringBufferAppendStr(b, expr->string.value, error));
        throws(tryStringBufferAppendChar(b, L'"', error));
      }
      else {
        throws(tryStringBufferAppendStr(b, expr->string.value, error));
      }
      break;
    }
    case N_SYMBOL: {
      throws(tryStringBufferAppendStr(b, expr->symbol.value, error));
      break;
    }
    case N_KEYWORD: {
      throws(tryStringBufferAppendChar(b, L':', error));
      throws(tryStringBufferAppendStr(b, expr->keyword.value, error));
      break;
    }
    case N_LIST: {
      throws(tryStringBufferAppendChar(b, L'(', error));

      ListElement *elem = expr->list.head;
      for (int i=0; i<expr->list.length; i++) {

        throws(tryExprPrnBufConf(elem->expr, b, readable, error));

        if (i + 1 < expr->list.length) {
          throws(tryStringBufferAppendChar(b, L' ', error));
        }

        elem = elem->next;
      }

      throws(tryStringBufferAppendChar(b, L')', error));
      break;
    }
    case N_MAP: {
      throws(tryStringBufferAppendChar(b, L'{', error));

      MapElement *elem = expr->map.head;
      for (int i=0; i<expr->map.length; i++) {

        throws(tryExprPrnBufConf(elem->key, b, readable, error));
        throws(tryStringBufferAppendChar(b, L' ', error));
        throws(tryExprPrnBufConf(elem->value, b, readable, error));

        if (i + 1 < expr->map.length) {
          throws(tryStringBufferAppendChar(b, L' ', error));
        }

        elem = elem->next;
      }

      throws(tryStringBufferAppendChar(b, L'}', error));
      break;
    }
    default:
    throwRuntimeError(error, "unsuported value type: %u", expr->type);
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryExprPrnBuf(Expr *expr, StringBuffer_t b, Error *error) {
  return tryExprPrnBufConf(expr, b, true, error);
}

RetVal tryExprPrnStr(Pool_t pool, Expr *expr, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(pool, &b, error));
  throws(tryExprPrnBuf(expr, b, error));

  wchar_t *output;
  throws(tryCopyText(pool, stringBufferText(b), &output, stringBufferLength(b), error));

  *ptr = output;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryExprPrn(Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  wchar_t *str = NULL;
  throws(tryExprPrnStr(pool, expr, &str, error));
  printf("%ls", str);

  return R_SUCCESS;

  failure:
  return ret;
}


