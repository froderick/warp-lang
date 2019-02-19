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

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

  throws(tryCopyText(input, &text, length, error));

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

  throws(tryStreamNext(stream, &token, error));

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
  throws(tryStringMake(text, len, &expr, error));

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
  RetVal ret;

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

  expr->type = N_NUMBER;
  expr->number.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryNumberRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(stream, &token, error));

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
  throws(tryNumberMake(value, &expr, error));

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

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

  throws(tryCopyText(name, &value, len, error));

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

  throws(tryStreamNext(stream, &token, error));

  if (token->type != T_SYMBOL) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_SYMBOL: %u", token->type);
  }

  Expr *expr;
  throws(trySymbolMake(token->text, token->source.length, &expr, error));

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

  throws(tryCopyText(name, &text, len, error));

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

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

  throws(tryStreamNext(stream, &token, error));

  if (token->type != T_KEYWORD) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_KEYWORD: %u", token->type);
  }

  uint64_t len = token->source.length - 1;
  wchar_t *text = token->text + 1;

  Expr *expr;
  throws(tryKeywordMake(text, len, &expr, error));

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
  RetVal ret;

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

  expr->type = N_BOOLEAN;
  expr->boolean.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryBooleanRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(stream, &token, error));

  if (token->type != T_TRUE && token->type != T_FALSE) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_TRUE or T_FALSE");
  }

  bool value = token->type == T_TRUE;

  Expr *expr;
  throws(tryBooleanMake(value, &expr, error));

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
  RetVal ret;

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

  expr->type = N_NIL;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryNilRead(TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(stream, &token, error));

  if (token->type != T_NIL) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_NIL: %u", token->type);
  }

  Expr *expr;
  throws(tryNilMake(&expr, error));

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

// Assume the opening paren has aready been read.
// Allocate a list, continue to read expressions and add them to it until a
// closed-paren is found.

RetVal tryListMake(Expr **ptr, Error *error) {
  RetVal ret;

  Expr *expr;
  tryMalloc(expr, sizeof(Expr), "Expr");

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

RetVal tryListRead(TokenStream_t stream, Expr **ptr, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Token *oParen = NULL, *cParen = NULL;
  Expr *expr = NULL;
  Expr *subexpr = NULL;

  // convenience

  throws(tryListMake(&expr, error));

  throws(tryStreamNext(stream, &oParen, error));

  if (oParen->type != T_OPAREN) {
    throwSyntaxError(error, oParen->source.position, "List must begin with ')': %u", oParen->type);
  }

  while (true) {

    ret = tryStreamPeek(stream, &cParen, error);
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
      tokenFree(oParen);
      tokenFree(cParen);
      break;
    }
    else { // read a new expression and add it to the list
      cParen = NULL;
      throws(tryExprRead(stream, &subexpr, error));
      throws(tryListAppend(&expr->list, subexpr, error));
      subexpr = NULL; // subexpr is part of list now
    }
  }

  return R_SUCCESS;

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
  RetVal ret;

  ListElement *elem;
  tryMalloc(elem, sizeof(ListElement), "ExprList");

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

  throws(tryStreamNext(stream, &token, error));

  if (token->type != T_QUOTE) {
    throwSyntaxError(error, token->source.position, "Quote must begin with ': %u", token->type);
  }

  throws(trySymbolMake(L"quote", wcslen(L"quote"), &quote, error));
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  throws(tryExprRead(stream, &subexpr, error));

  throws(tryListMake(&expr, error));

  throws(tryListAppend(&expr->list, quote, error));
  quote = NULL; // quote is now part of expr

  throws(tryListAppend(&expr->list, subexpr, error));
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

RetVal tryWrapperRead(TokenStream_t stream, wchar_t *symbolName, Expr **ptr, Error *error) {
  RetVal ret;

  Token *token;
  Expr *quote;
  Expr *subexpr;
  Expr *expr;

  throws(tryStreamNext(stream, &token, error));

//  if (token->type != tokenType) {
//    throwSyntaxError(error, token->source.position, "%ls must begin with %lc: %u", symbolName, startsWith, token->type);
//  }

  throws(trySymbolMake(symbolName, wcslen(symbolName), &quote, error));
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  throws(tryExprRead(stream, &subexpr, error));

  throws(tryListMake(&expr, error));

  throws(tryListAppend(&expr->list, quote, error));
  quote = NULL; // quote is now part of expr

  throws(tryListAppend(&expr->list, subexpr, error));
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

  throws(tryStreamPeek(stream, &peek, error));

  // discard comments from the token stream for now
  // maybe someday we'll use them for autodoc purposes
  while (peek->type == T_COMMENT) {

    Token *discard;
    throws(tryStreamNext(stream, &discard, error)); // discard the thing we peeked
    tokenFree(discard);
    peek = NULL;

    throws(tryStreamPeek(stream, &peek, error));
  }

  switch (peek->type) {

    // atoms
    case T_STRING:
      throws(tryStringRead(stream, ptr, error));
      break;
    case T_NUMBER:
      throws(tryNumberRead(stream, ptr, error));
      break;
    case T_SYMBOL:
      throws(trySymbolRead(stream, ptr, error));
      break;
    case T_KEYWORD:
      throws(tryKeywordRead(stream, ptr, error));
      break;
    case T_TRUE:
    case T_FALSE:
      throws(tryBooleanRead(stream, ptr, error));
      break;
    case T_NIL:
      throws(tryNilRead(stream, ptr, error));
      break;

    // lists
    case T_OPAREN:
      throws(tryListRead(stream, ptr, error));
      break;

    // reader macros
    case T_QUOTE:
      throws(tryQuoteRead(stream, ptr, error));
      break;
    case T_SYNTAX_QUOTE:
      throws(tryWrapperRead(stream, L"syntax-quote", ptr, error));
      break;
    case T_UNQUOTE:
      throws(tryWrapperRead(stream, L"unquote", ptr, error));
      break;
    case T_SPLICING_UNQUOTE:
      throws(tryWrapperRead(stream, L"splicing-unquote", ptr, error));
      break;

    default:
      throwSyntaxError(error, peek->source.position, "Unknown token type: %u", peek->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void exprFreeContents(Expr *expr) {
  if (expr != NULL) {
    if (expr->type == N_STRING) {
      stringFreeContents(&expr->string);
    } else if (expr->type == N_NUMBER) {
      // nothing to do
    } else if (expr->type == N_SYMBOL) {
      symbolFreeContents(&expr->symbol);
    } else if (expr->type == N_KEYWORD) {
      keywordFreeContents(&expr->keyword);
    } else if (expr->type == N_BOOLEAN) {
      // nothing to do
    } else if (expr->type == N_NIL) {
      // nothing to do
    } else if (expr->type == N_LIST) {
      listFreeContents(&expr->list);
    }
  }
}

void exprFree(Expr *expr) {
  if (expr != NULL) {
    exprFreeContents(expr);
    free(expr);
  }
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
      break;
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

RetVal _tryExprPrnStr(Expr *expr, StringBuffer_t b, Error *error) {
  RetVal ret;

  switch (expr->type) {
    case N_NIL:
    throws(tryStringBufferAppendStr(b, L"nil", error));
      break;
    case N_NUMBER: {
      wchar_t text[256];
      swprintf(text, sizeof(text), L"%llu", expr->number.value);
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
      throws(tryStringBufferAppendChar(b, L'"', error));
      throws(tryStringBufferAppendStr(b, expr->string.value, error));
      throws(tryStringBufferAppendChar(b, L'"', error));
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

        throws(_tryExprPrnStr(elem->expr, b, error));

        if (i + 1 < expr->list.length) {
          throws(tryStringBufferAppendChar(b, L' ', error));
        }

        elem = elem->next;
      }

      throws(tryStringBufferAppendChar(b, L')', error));
      break;
    }
    default:
      throwRuntimeError(error, "unsuported value type: %u", expr->type);
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryExprPrnStr(Expr *expr, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(&b, error));
  throws(_tryExprPrnStr(expr, b, error));

  wchar_t *output;
  throws(tryCopyText(stringBufferText(b), &output, stringBufferLength(b), error));
  stringBufferFree(b);

  *ptr = output;
  return R_SUCCESS;

  failure:
  stringBufferFree(b);
  return ret;
}

RetVal tryExprPrn(Expr *expr, Error *error) {
  RetVal ret;

  wchar_t *str = NULL;
  throws(tryExprPrnStr(expr, &str, error));
  printf("%ls", str);
  free(str);

  return R_SUCCESS;

  failure:
  return ret;
}


