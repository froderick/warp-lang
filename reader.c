#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>
#include <errno.h>

#include "utils.h"
#include "lexer.h"
#include "reader.h"

/*
 * Here is the basic AST implementation.
 */

RetVal tryStringMake(wchar_t *input, uint64_t length, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *text;

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  ret = tryCopyText(input, &text, length, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->type = N_STRING;
  expr->string.length = length;
  expr->string.value = text;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    if (text != NULL) {
      free(text);
    }
    return ret;
}

RetVal tryStringRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_STRING) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_STRING");
    goto failure;
  }

  if (token->source.length < 2) {
    ret = syntaxError(error, token->source.position, "Token should start and end with '\"'");
    goto failure;
  }

  // trim quotes
  wchar_t *text = token->text + 1;
  uint64_t len = token->source.length - 2;

  Expr *expr;
  ret = tryStringMake(text, len, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->source = token->source;

  *ptr = expr;
  tokenFree(token);
  return R_SUCCESS;

  failure:
    if (token != NULL) {
      tokenFree(token);
    }
    if (text != NULL) {
      free(text);
    }
    return ret;
}

void stringFreeContents(ExprString *string) {
  if (string != NULL) {
    if (string->value != NULL) {
      free(string->value);
    }
  }
}

RetVal tryNumberMake(uint64_t value, Expr **ptr, Error *error) {

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_NUMBER;
  expr->number.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryNumberRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_NUMBER) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_NUMBER");
    goto failure;
  }

  errno = 0;
  uint64_t value = wcstoull(token->text, NULL, 0);
  if (errno == EINVAL) {
    throwSyntaxError(error, token->source.position, "Token text does not represent a valid number: '%ls'", token->text);
  }
  if (errno == ERANGE) {
    ret = syntaxError(error, token->source.position, "Cannot represent a number literal larger than 64 bits unsigned");
    goto failure;
  }

  Expr *expr;
  ret = tryNumberMake(value, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->source = token->source;

  *ptr = expr;
  tokenFree(token);
  return R_SUCCESS;

  failure:
    if (token != NULL) {
      tokenFree(token);
    }
    return ret;
}

/*
 * This is for dynamically creating symbols, for instance to handle the reader
 * macros where certain tokens (like '`') expand into special forms.
 */
RetVal trySymbolMake(wchar_t *name, uint64_t len, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *value;

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  ret = tryCopyText(name, &value, len, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->type = N_SYMBOL;
  expr->symbol.value = value;
  expr->symbol.length = len;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    if (expr != NULL) {
      exprFree(expr);
    }
    if (value != NULL) {
      free(value);
    }
    return ret;
}

RetVal trySymbolRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_SYMBOL) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_SYMBOL");
    goto failure;
  }

  Expr *expr;
  ret = trySymbolMake(token->text, token->source.length, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->source = token->source;

  *ptr = expr;
  tokenFree(token);
  return R_SUCCESS;

  failure:
    if (token != NULL) {
      tokenFree(token);
    }
    return ret;
}

void symbolFreeContents(ExprSymbol *symbol) {
  if (symbol != NULL) {
    if (symbol->value != NULL) {
      free(symbol->value);
    }
  }
}

RetVal tryKeywordMake(wchar_t *name, uint64_t len, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *text;

  ret = tryCopyText(name, &text, len, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = N_KEYWORD;
  expr->keyword.length = len;
  expr->keyword.value = text;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    if (text != NULL) {
      free(text);
    }
    return ret;
}

RetVal tryKeywordRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_KEYWORD) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_KEYWORD");
    goto failure;
  }

  uint64_t len = token->source.length - 1;
  wchar_t *text = token->text + 1;

  Expr *expr;
  ret = tryKeywordMake(text, len, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->source = token->source;

  *ptr = expr;
  tokenFree(token);
  return R_SUCCESS;

  failure:
    if (token != NULL) {
      tokenFree(token);
    }
    return ret;
}

void keywordFreeContents(ExprKeyword *keyword) {
  if (keyword!= NULL) {
    if (keyword->value != NULL) {
      free(keyword->value);
    }
  }
}

RetVal tryBooleanMake(bool value, Expr **ptr, Error *error) {

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_BOOLEAN;
  expr->boolean.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryBooleanRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_TRUE && token->type != T_FALSE) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_TRUE or T_FALSE");
    goto failure;
  }

  bool value = token->type == T_TRUE;

  Expr *expr;
  ret = tryBooleanMake(value, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->source = token->source;

  *ptr = expr;
  tokenFree(token);
  return R_SUCCESS;

  failure:
  if (token != NULL) {
    tokenFree(token);
  }
  return ret;
}

RetVal tryNilMake(Expr **ptr, Error *error) {

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_NIL;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryNilRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_NIL) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_NIL");
    goto failure;
  }

  Expr *expr;
  ret = tryNilMake(&expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr->source = token->source;

  *ptr = expr;
  tokenFree(token);
  return R_SUCCESS;

  failure:
    if (token != NULL) {
      tokenFree(token);
    }
    return ret;
}

// Lists

RetVal tryExprRead(TokenStream_t stream, Expr **ptr, Error *error);
RetVal tryListAppend(ExprList *list, Expr *expr, Error *error);

// Assume the opening paren has aready been read.
// Allocate a list, continue to read expressions and add them to it until a
// closed-paren is found.

RetVal tryListMake(Expr **ptr, Error *error) {

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_LIST;
  expr->list.length = 0;

  // valid for zero length list
  expr->list.head = NULL;
  expr->list.tail = NULL;

  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryListRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;

  // these get cleaned up on failure
  Token *oParen = NULL, *cParen = NULL;
  Expr *expr = NULL;
  Expr *subexpr = NULL;

  // convenience

  ret = tryListMake(&expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryStreamNext(stream, &oParen, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (oParen->type != T_OPAREN) {
    ret = syntaxError(error, oParen->source.position, "List must begin with ')'");
    goto failure;
  }

  while (true) {

    ret = tryStreamPeek(stream, &cParen, error);
    if (ret != R_SUCCESS) {
      if (ret == R_EOF) { // eof too soon
        ret = syntaxError(error, oParen->source.position, "Encountered EOF, was expecting ')'");
      }
      goto failure;
    }

    if (cParen->type == T_CPAREN) { // found our closed paren

      streamDropPeeked(stream); // consume cParen now that nothing else can fail

      expr->source = oParen->source;
      expr->source.length = cParen->source.position - oParen->source.position;

      *ptr = expr;
      tokenFree(oParen);
      tokenFree(cParen);
      return R_SUCCESS;
    }
    else { // read a new expression and add it to the list
      cParen = NULL;

      ret = tryExprRead(stream, &subexpr, error);
      if (ret != R_SUCCESS) {
        goto failure;
      }

      ret = tryListAppend(&expr->list, subexpr, error);
      if (ret != R_SUCCESS) {
        goto failure;
      }
      subexpr = NULL; // subexpr is part of list now
    }
  }

  failure:
    if (oParen != NULL) {
      tokenFree(oParen);
    }
    if (cParen != NULL) {
      tokenFree(cParen);
    }
    if (subexpr != NULL) {
      exprFree(subexpr);
    }
    if (expr != NULL) {
      exprFree(expr);
    }
    return ret;
}

RetVal tryListAppend(ExprList *list, Expr *expr, Error *error) {

  ListElement *elem =  malloc(sizeof(ListElement));
  if (elem == NULL) {
    return memoryError(error, "malloc ExprList");
  }

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
}

void listFreeContents(ExprList *list) {
  while (list->head != NULL) {
    ListElement *elem = list->head;
    list->head = list->head->next;
    exprFree(elem->expr);
    free(elem);
  }
}

RetVal tryQuoteRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;

  Token *token;
  Expr *quote;
  Expr *subexpr;
  Expr *expr;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_QUOTE) {
    ret = syntaxError(error, token->source.position, "Syntax quote must begin with '`'");
    goto failure;
  }

  ret = trySymbolMake(L"quote", wcslen(L"quote"), &quote, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  ret = tryExprRead(stream, &subexpr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryListMake(&expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryListAppend(&expr->list, quote, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
  quote = NULL; // quote is now part of expr

  ret = tryListAppend(&expr->list, subexpr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
  // subexpr is now part of expr

  *ptr = expr;
  return R_SUCCESS;

  failure:
    if (token != NULL) {
      tokenFree(token);
    }
    if (quote != NULL) {
      exprFree(quote);
    }
    if (subexpr != NULL) {
      exprFree(subexpr);
    }
    if (expr != NULL) {
      exprFree(expr);
    }
    return ret;
}

// read the first token off the stream
// if it is an open paren, parse a list
// if it is a symbol, create a symbol
// else, explode

RetVal tryExprRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *peek;

  ret = tryStreamPeek(stream, &peek, error);
  if (ret != R_SUCCESS) {
    return ret; // can be R_EOF or R_ERROR
  }

  switch (peek->type) {

    // atoms
    case T_STRING:
      ret = tryStringRead(stream, ptr, error);
      break;
    case T_NUMBER:
      ret = tryNumberRead(stream, ptr, error);
      break;
    case T_SYMBOL:
      ret = trySymbolRead(stream, ptr, error);
      break;
    case T_KEYWORD:
      ret = tryKeywordRead(stream, ptr, error);
      break;
    case T_TRUE:
    case T_FALSE:
      ret = tryBooleanRead(stream, ptr, error);
      break;
    case T_NIL:
      ret = tryNilRead(stream, ptr, error);
      break;

    // lists
    case T_OPAREN:
      ret = tryListRead(stream, ptr, error);
      break;

    // reader macros
    case T_QUOTE:
      ret = tryQuoteRead(stream, ptr, error);
      break;

    default:
      ret = syntaxError(error, peek->source.position, "Unknown token type");
  }

  if (ret != R_SUCCESS) {
    return ret;
  }

  return R_SUCCESS;
}

void exprFree(Expr *expr) {
  if (expr->type == N_STRING) {
    stringFreeContents(&expr->string);
  }
  else if (expr->type == N_NUMBER) {
    // nothing to do
  }
  else if (expr->type == N_SYMBOL) {
    symbolFreeContents(&expr->symbol);
  }
  else if (expr->type == N_KEYWORD) {
    keywordFreeContents(&expr->keyword);
  }
  else if (expr->type == N_BOOLEAN) {
    // nothing to do
  }
  else if (expr->type == N_NIL) {
    // nothing to do
  }
  else if (expr->type == N_LIST) {
    listFreeContents(&expr->list);
  }
  free(expr);
}

RetVal tryExprDeepCopy(Expr *from, Expr **ptr, Error *error) {

  RetVal ret;

  // these get cleaned up on failure
  Expr *to;
  Expr *listItem;

  switch (from->type) {

    // atoms
    case N_STRING:
      throws(tryStringMake(from->string.value, from->string.length, &to, error));
      break;
    case N_NUMBER:
      throws(tryNumberMake(from->number.value, &to, error));
      break;
    case N_SYMBOL:
      throws(trySymbolMake(from->symbol.value, from->symbol.length, &to, error));
      break;
    case N_KEYWORD:
      throws(tryKeywordMake(from->keyword.value, from->keyword.length, &to, error));
      break;
    case N_BOOLEAN:
      throws(tryBooleanMake(from->boolean.value, &to, error));
      break;
    case N_NIL:
      throws(tryNilMake(&to, error));
      break;
    case N_LIST: {
      throws(tryListMake(&to, error));

      ListElement *elem = from->list.head;
      while (elem != NULL) {

        throws(tryExprDeepCopy(elem->expr, &listItem, error));
        throws(tryListAppend(&to->list, listItem, error));
        listItem = NULL; // item is now part of list

        elem = elem->next;
      }
    }

    default:
      throwSyntaxError(error, from->source.position, "Unknown expr type '%i'", from->type);
  }

  to->source = from->source;
  *ptr = to;

  return R_SUCCESS;

  failure:
    if (to != NULL) {
      exprFree(to);
    }
    if (listItem != NULL) {
      exprFree(listItem);
    }
    return ret;
}

RetVal tryExprPrn(Expr* expr, FILE *file, Error *error) {

  // TODO: allocate an expanding buffer, print into that, copy it into a malloced string, free the buffer, return the malloced string
  // consider writing a twin to stream source called something like 'output stream' that can let you write to an abstraction

  RetVal ret;

  switch (expr->type) {

    // atoms
    case N_STRING:
      printf("\"%ls\"", expr->string.value);
      break;
    case N_NUMBER:
      printf("%llu", expr->number.value);
      break;
    case N_SYMBOL:
      printf("%ls", expr->symbol.value);
      break;
    case N_KEYWORD:
      printf(":%ls", expr->keyword.value);
      break;
    case N_BOOLEAN: {
      char *val;
      if (expr->boolean.value) {
        val = "true";
      } else {
        val = "false";
      }
      printf("%s", val);
      break;
    }
    case N_NIL:
      printf("nil");
      break;
    case N_LIST: {
      printf("(");
      ListElement *elem = expr->list.head;
      for (int i=0; i<expr->list.length; i++) {

        throws(tryExprPrn(elem->expr, file, error));

        if (i + 1 < expr->list.length) {
          printf(" ");
        }

        elem = elem->next;
      }
      printf(")");
      break;
    }

    default:
      throwSyntaxError(error, expr->source.position, "Unknown expr type '%i'", expr->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}






















