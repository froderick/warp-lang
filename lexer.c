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

bool tryBufferMake(StringBuffer **ptr) {

  StringBuffer *b;
  wchar_t *data;

  if (NULL == (b = malloc(sizeof(StringBuffer)))) {
    reportError(SYSTEM, E_OOM, L"failed to allocate memory for making a string buffer");
    goto error;
  }

  b->usedChars = 0;
  b->allocatedChars = 256;

  if (NULL == (data = malloc(bufferAllocatedBytes(b)))) {
    reportError(SYSTEM, E_OOM, L"failed to allocate memory for making a string buffer data array");
    goto error;
  }

  bzero(data, bufferAllocatedBytes(b));

  b->data = data;
  *ptr = b;
  return OK;

  error:
    free(b);
    free(data);
    return ERROR;
}

void bufferFree(StringBuffer *b) {
  if (b != NULL) {
    free(b->data);
  }
  free(b);
}

int tryBufferAppend(StringBuffer *b, wchar_t ch) {

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

bool tryBufferMakeString(StringBuffer *b, wchar_t** ptr) {
  wchar_t *text = malloc(sizeof(wchar_t) * (b->usedChars + 1));

  if (text == NULL) {
    reportError(SYSTEM, E_OOM, L"failed to allocate memory for making a string from a buffer");
    return ERROR;
  }

  wcsncpy(text, b->data, b->usedChars);
  text[b->usedChars] = L'\0';

  *ptr = text;
  return OK;
}

/*
 * Token Model and Lexer
 */

bool tryTokenMake(TokenType type, wchar_t *text, unsigned long position, unsigned long length, bool textAllocated,
                  Token **token) {

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

bool tryLexerStateMake(LexerState **ptr) {

  StringBuffer *b = NULL;
  LexerState *s = NULL;

  if (tryBufferMake(&b)) {
    goto error;
  }

  s = malloc(sizeof(LexerState));
  if (s == NULL) {
    reportError(SYSTEM, E_OOM, L"failed to allocate memory for making a LexerState");
    goto error;
  }

  s->position = 0;
  s->b = b;
  *ptr = s;
  return OK;

  error:
    bufferFree(b);
    free(s);
    return ERROR;
}

void lexerStateFree(LexerState_t s) {
  if (s != NULL) {
    bufferFree(s->b);
  }
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

bool tryReadNumber(FILE *stream, LexerState_t s, wchar_t first, Token **token) {
  tryBufferAppend(s->b, first);
  // keep reading until char is not numeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      return ERROR;
    }
    s->position = s->position + 1;

    if (iswdigit(ch)) {
      tryBufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        reportError(SYSTEM, E_EOF, L"eof");
        return ERROR;
      }
      s->position = s->position - 1;

      wchar_t *text;
      bool error = tryBufferMakeString(s->b, &text);
      if (error) {
        return ERROR;
      }

      return tryTokenMake(T_NUMBER, text, s->position, s->b->usedChars, true, token);
    }
  }
}

bool tryReadSymbol(FILE *stream, LexerState_t s, wchar_t first, Token **token) {
  tryBufferAppend(s->b, first);
  // keep reading until char is not alphanumeric, then push back
  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      reportError(SYSTEM, E_EOF, L"eof");
      return ERROR;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      tryBufferAppend(s->b, ch);
    }
    else {
      wint_t result = ungetwc(ch, stream);
      if (result == WEOF && s->b->usedChars == 0) {
        reportError(SYSTEM, E_EOF, L"eof");
        return ERROR;
      }
      s->position = s->position - 1;

      wchar_t *text;
      bool error = tryBufferMakeString(s->b, &text);
      if (error) {
        return ERROR;
      }

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

      return tryTokenMake(type, text, s->position, s->b->usedChars, true, token);
    }
  }
}

bool tryReadKeyword(FILE *stream, LexerState_t s, Token **token) {
  // keep reading until char is not alphanumeric, then push back

  while (true) {
      
    wint_t ch = fgetwc(stream);
    if (ch == WEOF && s->b->usedChars == 0) {
      reportError(SYSTEM, E_EOF, L"eof");
      return ERROR;
    }
    s->position = s->position + 1;

    if (iswalnum(ch)) {
      tryBufferAppend(s->b, ch);
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

      wchar_t *text;
      bool error = tryBufferMakeString(s->b, &text);
      if (error) {
        return ERROR;
      }

      return tryTokenMake(T_KEYWORD, text, s->position, s->b->usedChars, true, token);
    }
  }
}

bool tryReadString(FILE *stream, LexerState_t s, Token **token) {
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

      wchar_t *text;
      bool error = tryBufferMakeString(s->b, &text);
      if (error) {
        return ERROR;
      }

      return tryTokenMake(T_STRING, text, s->position, s->b->usedChars, true, token);
    }
    else {
      tryBufferAppend(s->b, ch);
      if (!escape && ch == L'\\') {
        escape = true;
      }
      else {
        escape = false;
      }
    }
  }
}

bool tryTokenRead(FILE *stream, LexerState_t s, Token **token) {

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
    return tryTokenMake(T_OPAREN, L"(", s->position, 1, false, token);
  }
  else if (ch == L')') {
    return tryTokenMake(T_CPAREN, L")", s->position, 1, false, token);
  }
  else if (ch == L'\'') {
    return tryTokenMake(T_QUOTE, L"'", s->position, 1, false, token);
  }

  // multi-character tokens
  else if (iswdigit(ch)) {
    return tryReadNumber(stream, s, ch, token);
  }
  else if (iswalpha(ch)) {
    return tryReadSymbol(stream, s, ch, token);
  }
  else if (ch == L':') {
    return tryReadKeyword(stream, s, token);
  }
  else if (ch == L'"') {
    return tryReadString(stream, s, token);
  }

  // invalid token
  else {
    reportError(SYSTEM, E_INVALID_TOKEN, L"encountered invalid token: ...");
    return ERROR;
  }
}

bool tryTokensMake(Tokens **ptr) {

  Tokens *l;
  Token **tokens;

  if (NULL == (l = malloc(sizeof(Tokens)))) {
    reportError(SYSTEM, E_OOM, L"failed to create Tokens");
    goto error;
  }

  l->used = 0;
  l->size = 256;

  if (NULL == (tokens = malloc(l->size * sizeof(Token)))) {
    reportError(SYSTEM, E_OOM, L"failed to create Tokens buffer array");
    goto error;
  }

  l->data = tokens;
  bzero(l->data, l->size * sizeof(Token));

  *ptr = l;
  return OK;

  error:
    free(l);
    free(tokens);
    return ERROR;
}

void tokensFree(Tokens *l) {
  for (unsigned long i=0; i < l->used; i++) {
    tokenFree(l->data[i]);
  }
  free(l->data);
  free(l);
}

bool tryTokensAdd(Tokens *l, Token *t) {

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

bool tryTokensRead(FILE *stream, Tokens **ptr) {

  LexerState *s;
  Tokens *tokens;
  Token* t;

  if (tryLexerStateMake(&s)) {
    goto error;
  }

  if (tryTokensMake(&tokens)) {
    goto error;
  }

  while (1) {

    t = NULL;

    if (tryTokenRead(stream, s, &t)) {
      reportError(SYSTEM, E_UNKNOWN, L"error reading token from stream, giving up");
      goto error;
    }

    if (t == NULL) { // reached end of the stream, no tokens remain to be read
      break;
    }
    else {
      if (tryTokensAdd(tokens, t)) {
        reportError(SYSTEM, E_UNKNOWN, L"failed to add token to tokens buffer");
        goto error;
      }
      else {
        t = NULL; // avoid double-free on error
      }
    }
  }

  *ptr = tokens;
  return OK;

  error:
    lexerStateFree(s);
    tokensFree(tokens);
    tokenFree(t);
    return ERROR;
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
