#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>
#include <stdarg.h>

#include "lexer.h"
#include "parser.h"

/*
  expr : atom | sexpr
  sexpr : '(' expr* ')'
  atom : nil boolean symbol keyword string number
  boolean : T_TRUE | T_FALSE
  symbol : T_SYMBOL
  keyword : T_KEYWORD
  number : T_NUMBER
  string: T_STRING
*/

typedef enum AstExprType {ATOM, SEXPR} AstExprType;
typedef enum AstAtomType {NIL, BOOLEAN, SYMBOL, KEYWORD, STRING, NUMBER} AstAtomType;

typedef struct AstExpr AstExpr;
typedef struct AstSexpr AstSexpr;
typedef struct AstAtom AstAtom;

typedef struct AstSexprElement AstSexprElement;

// NOTE: ast nodes only include tokens if they were not dynamically created
// otherwise tokens are NULL

struct AstExpr {
  AstExprType type;
  union {
    AstAtom *atom;
    AstSexpr *sexpr;
  };
};

struct AstSexprElement {
  AstExpr *expr;
  AstSexprElement *next;
};

struct AstSexpr {
  Token *lparen;
  AstSexprElement *first;
  Token *rparen;
};

struct AstAtom {
  AstAtomType type;
  Token *token;
  union {
    bool bval;
    wchar_t *sval;
  };
};

AstExpr* makeExpr() {
  return malloc(sizeof(AstExpr));
}

AstSexpr* makeSexpr() {
  return malloc(sizeof(AstSexpr));
}

AstSexprElement* makeSexprElement() {
  return malloc(sizeof(AstSexprElement));
}

AstAtom* makeAtom(AstAtomType type, Token* token) {
  AstAtom* atom = malloc(sizeof(AstAtom));
  atom->type = type;
  atom->token = token;
  return atom;
}

void freeAtom(AstAtom *atom) {
  if (atom->token != NULL) {
    free(atom->token);
  }
  switch (atom->type) {

  case NIL:
  case BOOLEAN:
    break; // nothing to do

  case SYMBOL:
  case KEYWORD:
  case STRING:
    free(atom->sval);
    break;

  case NUMBER:

    break;

  // TODO: unhandled?
  }
}

// TODO: investigate C11 way of doing wide chars
// TODO: is it ok to make the lexer data structures part of the AST?
//       seems like we might want to deep copy the data we care about so we can free the tokens when we're done with them?

AstExpr* makeSymbol(wchar_t *name) {

  AstAtom* atom = malloc(sizeof(AstAtom));
  atom->type = BOOLEAN;
  atom->sval = name;
  atom->token = NULL;

  AstExpr* expr = makeExpr();
  expr->type = ATOM;
  expr->atom = atom;
  return expr;
}

// take in a sequence of tokens, model a queue for peek/pop
// model all the ast data structures in c
// parse tokens into the ast
// be able to print out the ast as s-expressions

typedef struct TokenCursor {
  Tokens* tokens;
  unsigned long current;
} TokenCursor;

TokenCursor* makeCursor(Tokens *tokens) {
  TokenCursor *cursor = malloc(sizeof(TokenCursor));
  cursor->tokens = tokens;
  cursor->current = 0;
  return cursor;
}

void freeCursor(TokenCursor* cursor) {
  free(cursor);
}

bool cursorFinished(TokenCursor* cursor) {
  return cursor->current == cursor->tokens->used;
}

TokenType peekCursor(TokenCursor* cursor, Token* ret) {
  if (cursorFinished(cursor)) {
    return T_
  }
  return cursor->tokens->data[cursor->current];
}

Token* popCursor(TokenCursor* cursor) {
  if (cursorFinished(cursor)) {
    return NULL;
  }
  cursor->current = cursor->current + 1;
  return cursor->tokens->data[cursor->current];
}


//typedef enum TokenType {
//  T_OPAREN,
//  T_CPAREN,
//  T_TRUE,
//  T_FALSE,
//  T_NIL,
//  T_QUOTE,
//  // value tokens
//  T_NUMBER,
//  T_STRING,
//  T_SYMBOL,
//  T_KEYWORD
//} TokenType;

AstExpr* parseAtom(TokenCursor* cursor) {
  Token* token = peekCursor(cursor);

  AstAtom *atom = NULL;
  switch (token->type) {
  case T_TRUE:
    atom = makeAtom(BOOLEAN, token);
    atom->bval = true;
    break;

  case T_FALSE:
    atom = makeAtom(BOOLEAN, token);
    atom->bval = false;
    break;

  case T_NIL:
    atom = makeAtom(NIL, token);
    break;

  case T_NUMBER:
    atom = makeAtom(NUMBER, token);
    atom->sval = wcsdup(token->text);
    break;

  case T_STRING:
    atom = makeAtom(STRING, token);
    atom->sval = wcsdup(token->text);
    break;

  case T_QUOTE:
    atom = makeAtom(SYMBOL, token);
    atom->sval = wcsdup(token->text);
    break;

  case T_SYMBOL:
    atom = makeAtom(SYMBOL, token);
    atom->sval = wcsdup(token->text);
    break;

  case T_KEYWORD:
    atom = makeAtom(KEYWORD, token);
    atom->sval = wcsdup(token->text);
    break;

  // TODO: unhandled?
  }

  if (atom == NULL) {
    // TODO: error?
  }

  popCursor(cursor);

  AstExpr *expr = makeExpr();
  expr->type = ATOM;
  expr->atom = atom;

  return expr;
}

bool isQuote(AstExpr* expr) {
  return expr->type == ATOM
    && expr->atom->type == SYMBOL
    && wcscmp(expr->atom->token->text, L"'") == 0;
}

AstSexprElement* cons(AstExpr *first, AstSexprElement *rest) {
  if (rest == NULL) {
    AstSexprElement *e = makeSexprElement();
    e->expr = first;
    return e;
  }
  else {
    AstSexprElement *e = makeSexprElement();
    e->expr = first;
    e->next = rest;
    return e;
  }
}

/**
 * Turns a variable number of AstExpr* arguments into an
 * AstExpr* containing a Sexpr of those arguments.
 */
AstExpr* list(AstExpr *args, ...) {

  AstSexpr *sexpr = makeSexpr();
  AstSexprElement *current = NULL;

  va_list ap;
  va_start(ap, args);

  while (args != 0) {

    // allocate next element, make `current` point to it
    if (current == NULL) {
      current = makeSexprElement();
      sexpr->first = current;
    }
    else {
      current->next = makeSexprElement();
      current = current->next;
    }
    current->expr = args;

    args = va_arg(ap, AstExpr*);
  }

  va_end(ap);

  AstExpr *expr = makeExpr();
  expr->type = SEXPR;
  expr->sexpr = sexpr;

  return expr;
}

AstExpr* _parseExpr(TokenCursor* cursor);

AstExpr* parseSexpr(TokenCursor* cursor) {
  
  AstSexpr *sexpr = makeSexpr();
  sexpr->lparen = popCursor(cursor);

  AstSexprElement *current = NULL;

  // TODO: NPE?
  while (peekCursor(cursor)->type != T_CPAREN) {
    // allocate next element, make `current` point to it
    if (current == NULL) {
      current = makeSexprElement();
      sexpr->first = current;
    }
    else {
      current->next = makeSexprElement();
      current = current->next;
    }
    current->expr = _parseExpr(cursor);
  }

  sexpr->rparen = popCursor(cursor);

  AstExpr *expr = makeExpr();
  expr->type = SEXPR;
  expr->sexpr = sexpr;

  return expr;
}

AstExpr* _parseExpr(TokenCursor* cursor) {
  while (true) {
    Token *token = peekCursor(cursor);

    if (token->type == T_OPAREN) {
      return parseSexpr(cursor);
    }

    AstExpr* atom = parseAtom(cursor);
    if (isQuote(atom)) {
      return list(makeSymbol(L"quote"), _parseExpr(cursor));
    }

    return atom;
  }
}

typedef enum AstParseResult {
  SUCCESS,
  SYNTAX_ERROR,
  IO_ERROR
} AstParseResult;

typedef struct SyntaxError {
  TokenType type;
  wchar_t *text;
  unsigned long position;
  unsigned long length;
} SyntaxError;

typedef struct IOError {
  wchar_t *text;
} IOError;

typedef struct ParseError {
  wchar_t *message;
  AstParseResult type;
  union {
    SyntaxError err;
    Syntaxerr;
  };

} AstParseError;


AstParseResult parseExpr(Tokens* tokens) {
  TokenCursor *cursor = makeCursor(tokens);


  freeCursor(cursor);
}

void freeExpr(AstExpr* expr) {
  
}
