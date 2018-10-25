#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>

#include "errors.h"
#include "lexer.h"

#define SYSTEM "lexer"
#define E_IO "io"

#define SYSTEM "lexer"
#define E_EOF "eof"
#define E_OOM "oom"
#define E_INVALID_TOKEN "invalid-token"
#define E_UNKNOWN "unknown"

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

bool tokenMake(TokenType type, wchar_t *text, unsigned long position, unsigned long length, bool textAllocated, Token **token) {

  Token *t = malloc(sizeof(Token));

  if (t == NULL) {
    reportError(SYSTEM, E_OOM, L"failed to allocate memory for token");
    return ERROR;
  }

  t->type = type;
  t->text = text;
  t->position = position;
  t->length = length;
  t->textAllocated = textAllocated;

  *token = t;

  return OK;
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

bool readNumber(FILE* stream, LexerState_t s, wchar_t first, Token **token) {
  bufferAppend(s->b, first);
  // keep reading until char is not numeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      return ERROR;
    }
    s->position = s->position + 1;

    if (iswdigit(ch)) {
      bufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        reportError(SYSTEM, E_EOF, L"eof");
        return ERROR;
      }
      s->position = s->position - 1;

      wchar_t *text = bufferMakeString(s->b);
      return tokenMake(T_NUMBER, text, s->position, s->b->usedChars, true, token);
    }
  }
}

bool readSymbol(FILE* stream, LexerState_t s, wchar_t first, Token **token) {
  bufferAppend(s->b, first);
  // keep reading until char is not alphanumeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      reportError(SYSTEM, E_EOF, L"eof");
      return ERROR;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      bufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        reportError(SYSTEM, E_EOF, L"eof");
        return ERROR;
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

      return tokenMake(type, text, s->position, s->b->usedChars, true, token);
    }
  }
}

bool readKeyword(FILE* stream, LexerState_t s, Token **token) {
  // keep reading until char is not alphanumeric, then push back

  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      reportError(SYSTEM, E_EOF, L"eof");
      return ERROR;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      bufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        reportError(SYSTEM, E_EOF, L"eof");
        return ERROR;
      }
      s->position = s->position - 1;

      if (s->b->usedChars == 0) {
        reportError(SYSTEM, E_INVALID_TOKEN, L"invalid token");
        return ERROR;
      }

      wchar_t *text = bufferMakeString(s->b);
      return tokenMake(T_KEYWORD, text, s->position, s->b->usedChars, true, token);
    }
  }
}

bool readString(FILE* stream, LexerState_t s, Token **token) {
  // keep reading until char is a non-escaped quote

  bool escape = false;

  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      reportError(SYSTEM, E_EOF, L"eof");
      return ERROR;
    }
    s->position = s->position + 1;

    if (!escape && ch == L'"') {
      wchar_t *text = bufferMakeString(s->b);
      return tokenMake(T_STRING, text, s->position, s->b->usedChars, true, token);
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

bool tokenRead(FILE *stream, LexerState_t s, Token **token) {

  bufferClear(s->b);

  wint_t ch = fgetwc(stream);
  if (ch == WEOF) {

    if (feof(stream)) {
      *token = NULL;
      return OK; // end of stream, no tokens left to parse
    }
    else {
      reportErrnoError(SYSTEM, "stream token read error");
      return ERROR;
    }
  }
  s->position = s->position + 1;

  while (isWhitespace(ch)) {
    ch = fgetwc(stream);
    if (ch == WEOF) {
      reportError(SYSTEM, E_EOF, L"put eof error here");
      return ERROR;
    }
    s->position = s->position + 1;
  }

  // single-character tokens, do not require buffering
  if (ch == L'(') {
    return tokenMake(T_OPAREN, L"(", s->position, 1, false, token);
  }
  else if (ch == L')') {
    return tokenMake(T_CPAREN, L")", s->position, 1, false, token);
  }
  else if (ch == L'\'') {
    return tokenMake(T_QUOTE, L"'", s->position, 1, false, token);
  }

  // multi-character tokens
  else if (iswdigit(ch)) {
    return readNumber(stream, s, ch, token);
  }
  else if (iswalpha(ch)) {
    return readSymbol(stream, s, ch, token);
  }
  else if (ch == L':') {
    return readKeyword(stream, s, token);
  }
  else if (ch == L'"') {
    return readString(stream, s, token);
  }

  // invalid token
  else {
    reportError(SYSTEM, E_INVALID_TOKEN, L"encountered invalid token: ...");
    return ERROR;
  }
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

bool tokensAdd(Tokens *l, Token *t) {

  if (l->used == l->size) {
    l->size = l->size * 2;
    l->data= realloc(l->data, l->size * sizeof(Token));

    if (l->data == NULL) {
      reportError(SYSTEM, E_OOM, L"failed to double size of token buffer via realloc");
      return ERROR;
    }
  }

  l->data[l->used] = t;
  l->used = l->used + 1;

  return OK;
}

bool tokensRead(FILE *stream, Tokens** tokens) {
  LexerState_t s = lexerStateMake();
  *tokens = tokensMake();

  Token* t;
  bool error;
  while (1) {

    error = tokenRead(stream, s, &t);
    if (error) {
      reportError(SYSTEM, E_UNKNOWN, L"error reading token from stream, giving up");
      return ERROR;
    }

    if (t == NULL) { // reached end of the stream, no tokens remain to be read
      break;
    }
    else {
      error = tokensAdd(*tokens, t);
      if (error) {
        reportError(SYSTEM, E_UNKNOWN, L"failed to add token to tokens buffer");
        return ERROR;
      }
    }
  }

  lexerStateFree(s);

  return OK;
}

const char* tokenName(TokenType type) {
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
