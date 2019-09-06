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

unsigned long allocatedBytes(StringBuffer *buf) {
  return sizeof(wchar_t) * buf->allocatedChars;
}

unsigned long usedBytes(StringBuffer *buf) {
  return sizeof(wchar_t) * buf->usedChars;
}

StringBuffer* makeBuffer() {
  StringBuffer *b = malloc(sizeof(StringBuffer));
  b->usedChars = 0;
  b->allocatedChars = 256;
  b->data = malloc(allocatedBytes(b));
  bzero(b->data, allocatedBytes(b));
  return b;
}

void freeBuffer(StringBuffer *b) {
  free(b->data);
  free(b);
}

int append(StringBuffer *b, wchar_t ch) {

  if (b->usedChars + 1 == (b->allocatedChars - 1)) {  


    unsigned long oldSizeInBytes = allocatedBytes(b);
    unsigned long newSizeInBytes = oldSizeInBytes * 2;

    b->data= realloc(b->data, newSizeInBytes);
    b->allocatedChars = b->allocatedChars * 2;
  }

  b->data[b->usedChars] = ch;
  b->usedChars = b->usedChars + 1;

  return 0;
}

void clear(StringBuffer *b) {
  bzero(b->data, usedBytes(b));
  b->usedChars = 0;
}

/*
 * Token Model and Lexer
 */

Token* makeToken(TokenType type, wchar_t *text, unsigned long position, unsigned long length) {
  Token *t = malloc(sizeof(Token));
  t->type = type;
  t->text = text;
  t->position = position;
  t->length = length;
  return t;
}

void freeToken(Token *t) {
  free(t->text);
  free(t);
}

typedef struct LexerState {
  unsigned long position;
  StringBuffer *b;
} LexerState;

LexerState_t makeLexerState() {
  LexerState_t s = malloc(sizeof(LexerState));
  s->position = 0;
  s->b = makeBuffer();
  return s;
}

void freeLexerState(LexerState_t s) {
  free(s->b);
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
  append(s->b, first);
  // keep reading until char is not numeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;

    if (iswdigit(ch)) {
      append(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        *err = ERR_EOF;
        return NULL;
      }
      s->position = s->position - 1;

      wchar_t *text = malloc(usedBytes(s->b) + 1);
      wcsncpy(text, s->b->data, s->b->usedChars);
      Token* t = makeToken(T_NUMBER, text, s->position, s->b->usedChars);
      return t;
    }
  }
}

Token* readSymbol(FILE* stream, LexerState_t s, int *err, wchar_t first) {
  append(s->b, first);
  // keep reading until char is not alphanumeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      *err = ERR_EOF;
      return NULL;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      append(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        *err = ERR_EOF;
        return NULL;
      }
      s->position = s->position - 1;

      wchar_t *text = malloc(usedBytes(s->b) + 1);
      unsigned long textLength = s->b->usedChars;
      wcsncpy(text, s->b->data, textLength);

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

      return makeToken(type, text, s->position, textLength);
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
      append(s->b, ch);
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

      wchar_t *text = malloc(usedBytes(s->b) + 1);
      wcsncpy(text, s->b->data, s->b->usedChars);
      Token* t = makeToken(T_KEYWORD, text, s->position, s->b->usedChars);
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
      wchar_t *text = malloc(usedBytes(s->b) + 1);
      unsigned long textLength = s->b->usedChars;
      wcsncpy(text, s->b->data, textLength);
      return makeToken(T_STRING, text, s->position, textLength);
    }
    else {
      append(s->b, ch);
      if (!escape && ch == L'\\') {
        escape = true;
      }
      else {
        escape = false;
      }
    }
  }
}

wchar_t* makeString(wchar_t *s) {
  wchar_t *text = malloc((wcslen(s) * sizeof(wchar_t)) + 1);
  wcsncpy(text, s, wcslen(s));
  return text;
}

Token* readToken(FILE* stream, LexerState_t s, int *err) {

  clear(s->b);

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
    wchar_t *text = malloc(usedBytes(s->b) + 1);
    wcsncpy(text, s->b->data, s->b->usedChars);
    return makeToken(T_OPAREN, makeString(L"("), s->position, 1);
  }
  if (ch == L')') {
    return makeToken(T_CPAREN, makeString(L")"), s->position, 1);
  }
  if (ch == L'\'') {
    return makeToken(T_QUOTE, makeString(L"'"), s->position, 1);
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

Tokens* makeTokens() {
  Tokens *l = malloc(sizeof(Tokens));
  l->used = 0;
  l->size = 256;
  l->data = malloc(l->size * sizeof(Token));
  bzero(l->data, l->size * sizeof(Token));
  return l;
}

void freeTokens(Tokens *l) {
  for (unsigned long i=0; i < l->used; i++) {
    freeToken(l->data[i]);
  }
  free(l->data);
  free(l);
}

int addToken(Tokens *l, Token *t) {

  if (l->used == l->size) {
    l->size = l->size * 2;
    l->data= realloc(l->data, l->size * sizeof(Token));
  }

  l->data[l->used] = t;
  l->used = l->used + 1;

  return 0;
}

Tokens* readTokens(FILE* stream, int *err) {
  LexerState_t s = makeLexerState();
  Tokens *tokens = makeTokens();

  Token* t;
  while (1) {
    t = readToken(stream, s, err);
    if (t == NULL) {
      break;
    }
    else {
      addToken(tokens, t);
    }
  }

  freeLexerState(s);
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