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
 * Reader Begins
 */

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

  Expr *expr = stringMake(pool, text, len);
  expr->source = token->source;

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

  Expr *expr = numberMake(pool, value);
  expr->source = token->source;

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCharRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  throws(tryStreamNext(pool, stream, &token, error));

  if (token->type != T_CHAR) {
    throwSyntaxError(error, token->source.position, "Token is not a type of T_CHAR: %u", token->type);
  }

  if (wcslen(token->text) != 1) {
    throwSyntaxError(error, token->source.position, "Token of type T_CHAR must be 1 in length: %lu",
        wcslen(token->text));
  }

  Expr *expr = charMake(pool, token->text[0]);
  expr->source = token->source;

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

  Expr *expr = symbolMake(pool, token->text, token->source.length);
  expr->source = token->source;

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

  Expr *expr = keywordMake(pool, text, len);
  expr->source = token->source;

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

  Expr *expr = booleanMake(pool, value);
  expr->source = token->source;

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

  Expr *expr = nilMake(pool);
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

RetVal tryListRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Token *oParen = NULL, *cParen = NULL;
  Expr *expr = NULL;
  Expr *subexpr = NULL;

  // convenience

  expr = listMake(pool);

  throws(tryStreamNext(pool, stream, &oParen, error));

  if (oParen->type != T_OPAREN) {
    throwSyntaxError(error, oParen->source.position, "List must begin with '(': %u", oParen->type);
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
      listAppend(pool, &expr->list, subexpr);
      subexpr = NULL; // subexpr is part of list now
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryVecRead(Pool_t pool, TokenStream_t stream, Expr **ptr, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Token *oParen = NULL, *cParen = NULL;
  Expr *expr = NULL;
  Expr *subexpr = NULL;

  // convenience

  expr = vecMake(pool);

  throws(tryStreamNext(pool, stream, &oParen, error));

  if (oParen->type != T_OVEC) {
    throwSyntaxError(error, oParen->source.position, "Vector must begin with '[': %u", oParen->type);
  }

  while (true) {

    ret = tryStreamPeek(pool, stream, &cParen, error);
    if (ret != R_SUCCESS) {
      if (ret == R_EOF) { // eof too soon
        throwSyntaxError(error, oParen->source.position, "Encountered EOF, was expecting ']'");
      }
      goto failure;
    }

    if (cParen->type == T_CVEC) { // found our closed paren

      streamDropPeeked(stream); // consume cParen now that nothing else can fail

      expr->source = oParen->source;
      expr->source.length = cParen->source.position - oParen->source.position;

      *ptr = expr;
      break;
    }
    else { // read a new expression and add it to the vec
      cParen = NULL;
      throws(tryExprRead(pool, stream, &subexpr, error));
      vecAppend(pool, &expr->vec, subexpr);
      subexpr = NULL; // subexpr is part of vec now
    }
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

  expr = mapMake(pool);

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
      mapPut(pool, &expr->map, key, value);
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

  quote = symbolMake(pool, L"quote", wcslen(L"quote"));
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  throws(tryExprRead(pool, stream, &subexpr, error));

  expr = listMake(pool);

  listAppend(pool, &expr->list, quote);
  quote = NULL; // quote is now part of expr

  listAppend(pool, &expr->list, subexpr);
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

  quote = symbolMake(pool, symbolName, wcslen(symbolName));
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  throws(tryExprRead(pool, stream, &subexpr, error));

  expr = listMake(pool);

  listAppend(pool, &expr->list, quote);
  quote = NULL; // quote is now part of expr

  listAppend(pool, &expr->list, subexpr);
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
    case T_CHAR:
      throws(tryCharRead(pool, stream, ptr, error));
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

    // vectors
    case T_OVEC:
    throws(tryVecRead(pool, stream, ptr, error));
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



