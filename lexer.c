#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>

#include "lexer.h"

/*
 * Simple auto-expanding string buffer implementation.
 * Built on wide chars, so UTF8 friendly.
 */

typedef struct StringBuffer {
  wchar_t *data;
  unsigned long allocatedChars;
  unsigned long usedChars;
} StringBuffer;

unsigned long bufferAllocatedBytes(StringBuffer *buf) {
  return sizeof(wchar_t) * buf->allocatedChars;
}

unsigned long bufferUnusedBytes(StringBuffer *buf) {
  return sizeof(wchar_t) * buf->usedChars;
}

StringBuffer* bufferMake() {
  StringBuffer *b = malloc(sizeof(StringBuffer));
  b->usedChars = 0;
  b->allocatedChars = 256;
  b->data = malloc(bufferAllocatedBytes(b));
  bzero(b->data, bufferAllocatedBytes(b));
  return b;
}

void bufferFree(StringBuffer *b) {
  free(b->data);
  free(b);
}

int bufferAppend(StringBuffer *b, wchar_t ch) {

  if (b->usedChars + 1 == (b->allocatedChars - 1)) {  

    unsigned long oldSizeInBytes = bufferAllocatedBytes(b);
    unsigned long newSizeInBytes = oldSizeInBytes * 2;

    b->data= realloc(b->data, newSizeInBytes);
    b->allocatedChars = b->allocatedChars * 2;
  }

  b->data[b->usedChars] = ch;
  b->usedChars = b->usedChars + 1;

  return 0;
}

void bufferClear(StringBuffer *b) {
  bzero(b->data, bufferUnusedBytes(b));
  b->usedChars = 0;
}

wchar_t* bufferMakeString(StringBuffer *b) {
  wchar_t *text = malloc(sizeof(wchar_t) * (b->usedChars + 1));
  wcsncpy(text, b->data, b->usedChars);
  text[b->usedChars] = L'\0';
  return text;
}

/*
 * Token Model and Lexer
 */

Token* tokenMake(TokenType type, wchar_t *text, unsigned long position, unsigned long length, bool textAllocated) {
  Token *t = malloc(sizeof(Token));
  t->type = type;
  t->text = text;
  t->position = position;
  t->length = length;
  t->textAllocated = textAllocated;
  return t;
}

void tokenFree(Token *t) {
  if (t->textAllocated) {
    free(t->text);
  }
  free(t);
}

typedef struct LexerState {
  unsigned long position;
  StringBuffer *b;
} LexerState;

LexerState_t lexerStateMake() {
  LexerState_t s = malloc(sizeof(LexerState));
  s->position = 0;
  s->b = bufferMake();
  return s;
}

void lexerStateFree(LexerState_t s) {
  bufferFree(s->b);
  free(s);
}

const int ERR_EOF = 1;
const int ERR_UNEXPECTED_EOF = 2;
const int ERR_INVALID_TOKEN = 3;

bool isWhitespace(wchar_t ch) {
  return ch == L'\n'
      || ch == L' '
      || ch == L'\t';
}

bool isNil(wchar_t *text) {
  return wcscmp(text, L"nil") == 0;
}

bool isTrue(wchar_t *text) {
  return wcscmp(text, L"true") == 0;
}

bool isFalse(wchar_t *text) {
  return wcscmp(text, L"false") == 0;
}

Token* readNumber(FILE* stream, LexerState_t s, int *err, wchar_t first) {
  bufferAppend(s->b, first);
  // keep reading until char is not numeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;

    if (iswdigit(ch)) {
      bufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        *err = ERR_EOF;
        return NULL;
      }
      s->position = s->position - 1;

      wchar_t *text = bufferMakeString(s->b);
      Token* t = tokenMake(T_NUMBER, text, s->position, s->b->usedChars, true);
      return t;
    }
  }
}

Token* readSymbol(FILE* stream, LexerState_t s, int *err, wchar_t first) {
  bufferAppend(s->b, first);
  // keep reading until char is not alphanumeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      bufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        *err = ERR_EOF;
        return NULL;
      }
      s->position = s->position - 1;

      wchar_t *text = bufferMakeString(s->b);

      TokenType type = T_NONE;
      if (isNil(text)) {
        type = T_NIL;
      }
      else if (isTrue(text)) {
        type = T_TRUE;
      }
      else if (isFalse(text)) {
        type = T_FALSE;
      }
      else {
        type = T_SYMBOL;
      }

      return tokenMake(type, text, s->position, s->b->usedChars, true);
    }
  }
}

Token* readKeyword(FILE* stream, LexerState_t s, int *err) {
  // keep reading until char is not alphanumeric, then push back

  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      bufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        *err = ERR_EOF;
        return NULL;
      }
      s->position = s->position - 1;

      if (s->b->usedChars == 0) {
        *err = ERR_INVALID_TOKEN;
        return NULL;
      }

      wchar_t *text = bufferMakeString(s->b);
      Token* t = tokenMake(T_KEYWORD, text, s->position, s->b->usedChars, true);
      return t;
    }
  }
}

Token* readString(FILE* stream, LexerState_t s, int *err) {
  // keep reading until char is a non-escaped quote

  bool escape = false;

  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;

    if (!escape && ch == L'"') {
      wchar_t *text = bufferMakeString(s->b);
      return tokenMake(T_STRING, text, s->position, s->b->usedChars, true);
    }
    else {
      bufferAppend(s->b, ch);
      if (!escape && ch == L'\\') {
        escape = true;
      }
      else {
        escape = false;
      }
    }
  }
}

Token* tokenRead(FILE *stream, LexerState_t s, int *err) {

  bufferClear(s->b);

  wint_t ch = fgetwc(stream);
  if (ch == WEOF) {
    *err = ERR_EOF;
    return NULL;
  }
  s->position = s->position + 1;

  while (isWhitespace(ch)) {
    ch = fgetwc(stream);
    if (ch == WEOF) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;
  }

  // single-character tokens, do not require buffering
  if (ch == L'(') {
    return tokenMake(T_OPAREN, L"(", s->position, 1, false);
  }
  if (ch == L')') {
    return tokenMake(T_CPAREN, L")", s->position, 1, false);
  }
  if (ch == L'\'') {
    return tokenMake(T_QUOTE, L"'", s->position, 1, false);
  }

  // multi-character tokens
  if (iswdigit(ch)) {
    return readNumber(stream, s, err, ch);
  }

  if (iswalpha(ch)) {
    return readSymbol(stream, s, err, ch);
  }

  if (ch == L':') {
    return readKeyword(stream, s, err);
  }

  if (ch == L'"') {
    return readString(stream, s, err);
  }

  *err = ERR_INVALID_TOKEN;
  return NULL;
}

Tokens* tokensMake() {
  Tokens *l = malloc(sizeof(Tokens));
  l->used = 0;
  l->size = 256;
  l->data = malloc(l->size * sizeof(Token));
  bzero(l->data, l->size * sizeof(Token));
  return l;
}

void tokensFree(Tokens *l) {
  for (unsigned long i=0; i < l->used; i++) {
    tokenFree(l->data[i]);
  }
  free(l->data);
  free(l);
}

int tokensAdd(Tokens *l, Token *t) {

  if (l->used == l->size) {
    l->size = l->size * 2;
    l->data= realloc(l->data, l->size * sizeof(Token));
  }

  l->data[l->used] = t;
  l->used = l->used + 1;

  return 0;
}

Tokens* tokensRead(FILE *stream, int *err) {
  LexerState_t s = lexerStateMake();
  Tokens *tokens = tokensMake();

  Token* t;
  while (1) {
    t = tokenRead(stream, s, err);
    if (t == NULL) {
      break;
    }
    else {
      tokensAdd(tokens, t);
    }
  }

  lexerStateFree(s);
  return tokens;
}

char* tokenName(TokenType type) {
  switch (type) {
  case T_OPAREN:
    return "OPAREN";
  case T_CPAREN:
    return "CPAREN";
  case T_QUOTE:
    return "QUOTE";
  case T_SYMBOL:
    return "SYMBOL";
  case T_TRUE:
    return "TRUE";
  case T_FALSE:
    return "FALSE";
  case T_NIL:
    return "NIL";
  case T_NUMBER:
    return "NUMBER";
  case T_STRING:
    return "STRING";
  case T_KEYWORD:
    return "KEYWORD";
  default:
    return "<UNKNOWN>";
  }
}
