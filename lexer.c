#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>
#include <errno.h>

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

bool tryBufferMake(StringBuffer **ptr) {

  StringBuffer *b;
  wchar_t *data;

  if (NULL == (b = malloc(sizeof(StringBuffer)))) {
    if (DEBUG) { printf("error: malloc-ing StringBuffer\n"); }
    goto error;
  }

  b->usedChars = 0;
  b->allocatedChars = 256;

  if (NULL == (data = malloc(bufferAllocatedBytes(b)))) {
    if (DEBUG) { printf("error: malloc-ing StringBuffer array\n"); }
    goto error;
  }

  bzero(data, bufferAllocatedBytes(b));

  b->data = data;
  *ptr = b;
  return LEX_SUCCESS;

  error:
    free(b);
    free(data);
    return LEX_ERROR;
}

void bufferFree(StringBuffer *b) {
  if (b != NULL) {
    free(b->data);
  }
  free(b);
}

// TODO: fix this malloc
int tryBufferAppend(StringBuffer *b, wchar_t ch) {

  if (b->usedChars + 1 == (b->allocatedChars - 1)) {

    unsigned long oldSizeInBytes = bufferAllocatedBytes(b);
    unsigned long newSizeInBytes = oldSizeInBytes * 2;

    b->data= realloc(b->data, newSizeInBytes);
    b->allocatedChars = b->allocatedChars * 2;
  }

  b->data[b->usedChars] = ch;
  b->usedChars = b->usedChars + 1;
  b->data[b->usedChars] = L'\0';

  return 0;
}

void bufferClear(StringBuffer *b) {
  bzero(b->data, bufferUnusedBytes(b));
  b->usedChars = 0;
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

/*
 * Token Model and Lexer
 */

int tryTokenInit(TokenType type, wchar_t *text, unsigned long position, unsigned long length, Token **ptr) {

  Token *t;

  t = malloc(sizeof(Token) + (sizeof(wchar_t) * length) + 1);
  if (t == NULL) {
    if (DEBUG) { printf("error: malloc-ing LexerState\n"); }
    return LEX_ERROR;
  }

  t->type = type;
  t->typeName = tokenName(type);
  t->position = position;
  t->length = length;
  wcpncpy(t->text, text, length);
  t->text[length] = L'\0';

  *ptr = t;

  return LEX_SUCCESS;
}

typedef struct LexerState {
  unsigned long position;
  StringBuffer *b;
} LexerState;

int tryTokenInitFromLexer(LexerState *s, TokenType type, Token **ptr) {
  return tryTokenInit(type, s->b->data, s->position, s->b->usedChars, ptr);
}

void tokenFree(Token *t) {
  free(t);
}

bool tryLexerStateMake(LexerState **ptr) {

  StringBuffer *b = NULL;
  LexerState *s = NULL;

  if (tryBufferMake(&b)) {
    goto error;
  }

  s = malloc(sizeof(LexerState));
  if (s == NULL) {
    if (DEBUG) { printf("error: malloc-ing LexerState\n"); }
    goto error;
  }

  s->position = 0;
  s->b = b;
  *ptr = s;
  return LEX_SUCCESS;

  error:
    bufferFree(b);
    free(s);
    return LEX_ERROR;
}

void lexerStateFree(LexerState *s) {
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

int tryReadChar(FILE *stream, LexerState *s, wchar_t* ch) {
  *ch = fgetwc(stream);
  if (*ch == WEOF) {
    if (feof(stream)) {
      return LEX_EOF;
    }
    else {
      if (DEBUG) { printf("error: failed to read token from stream ->  '%s'\n", strerror(errno)); }
      return LEX_ERROR;
    }
  }
  s->position = s->position + 1;
  return LEX_SUCCESS;
}

int tryUnreadChar(FILE *stream, LexerState *s, wchar_t ch) {
  wint_t result = ungetwc(ch, stream);
  if (result == WEOF && s->b->usedChars == 0) {
    if (DEBUG) { printf("error: failed to push character back onto stream"); }
    return LEX_ERROR;
  }
  s->position = s->position - 1;
  return LEX_SUCCESS;
}

int tryReadNumber(FILE *stream, LexerState *s, wchar_t first, Token **token) {
  tryBufferAppend(s->b, first);
  // keep reading until char is not numeric, then push back
  while (true) {
      
    wint_t ch;
    int read = tryReadChar(stream, s, &ch);
    if (read != LEX_SUCCESS) {
      return read;
    }

    if (iswdigit(ch)) {
      tryBufferAppend(s->b, ch);
    }
    else {

      int unread = tryUnreadChar(stream, s, ch);
      if (unread != LEX_SUCCESS) {
        return unread;
      }

      int error = tryTokenInitFromLexer(s, T_NUMBER, token);
      if (error != LEX_SUCCESS) {
        return LEX_ERROR;
      }

      return LEX_SUCCESS;
    }
  }
}

// TODO: apply all the bug fixes in here to the other tryRead* functions

bool tryReadSymbol(FILE *stream, LexerState *s, wchar_t first, Token **token) {
  tryBufferAppend(s->b, first);
  // keep reading until char is not alphanumeric, then push back

  bool eof;
  while (true) {

    wint_t ch;
    int read = tryReadChar(stream, s, &ch);

    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      break;
    }

    if (iswalnum(ch)) {
      tryBufferAppend(s->b, ch);
      continue;
    }

    if (tryUnreadChar(stream, s, ch) != LEX_SUCCESS) {
      return LEX_ERROR;
    }
    else {
      break;
    }
  }

  if (s->b->usedChars == 0) {
    if (DEBUG) { printf("error: symbol token type cannot be empty"); }
    return LEX_ERROR;
  }

  wchar_t *text = s->b->data;

  TokenType type;
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

  int error = tryTokenInitFromLexer(s, type, token);
  if (error != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

bool tryReadKeyword(FILE *stream, LexerState *s, Token **token) {
  // keep reading until char is not alphanumeric, then push back

  while (true) {

    wint_t ch;
    int read = tryReadChar(stream, s, &ch);
    if (read != LEX_SUCCESS) {
      return read;
    }

    if (iswalnum(ch)) {
      tryBufferAppend(s->b, ch);
    }
    else {

      int unread = tryUnreadChar(stream, s, ch);
      if (unread != LEX_SUCCESS) {
        return unread;
      }

      if (s->b->usedChars == 0) {
        if (DEBUG) { printf("error: keyword token type cannot be empty"); }
        return LEX_ERROR;
      }

      int error = tryTokenInitFromLexer(s, T_KEYWORD, token);
      if (error != LEX_SUCCESS) {
        return LEX_ERROR;
      }

      return LEX_SUCCESS;
    }
  }
}

bool tryReadString(FILE *stream, LexerState *s, Token **token) {
  // keep reading until char is a non-escaped quote

  bool escape = false;

  while (true) {

    wint_t ch;
    int read = tryReadChar(stream, s, &ch);
    if (read != LEX_SUCCESS) {
      return read;
    }

    if (!escape && ch == L'"') {

      int error = tryTokenInitFromLexer(s, T_STRING, token);
      if (error != LEX_SUCCESS) {
        return LEX_ERROR;
      }

      return LEX_SUCCESS;
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

int tryTokenRead(FILE *stream, LexerState *s, Token **token) {

  bufferClear(s->b);

  wint_t ch = fgetwc(stream);
  if (ch == WEOF) {
    if (feof(stream)) {
      return LEX_EOF; // end of stream, no tokens left to parse
    }
    else {
      if (DEBUG) { printf("error: error reading tokenf from stream -> '%s'\n", strerror(errno)); }
      return LEX_ERROR;
    }
  }
  s->position = s->position + 1;

  while (isWhitespace(ch)) {
    ch = fgetwc(stream);
    if (ch == WEOF) {
      if (feof(stream)) {
        return LEX_EOF;
      }
      else {
        if (DEBUG) { printf("error: error reading tokenf from stream -> '%s'\n", strerror(errno)); }
        return LEX_ERROR;
      }
    }
    s->position = s->position + 1;
  }

  // single-character tokens, do not require buffering
  if (ch == L'(') {
    int error = tryTokenInit(T_OPAREN, L"(", s->position, 1, token);
    if (error != LEX_SUCCESS) {
      return LEX_ERROR;
    }
    return LEX_SUCCESS;
  }
  else if (ch == L')') {
    int error = tryTokenInit(T_CPAREN, L")", s->position, 1, token);
    if (error != LEX_SUCCESS) {
      return LEX_ERROR;
    }
    return LEX_SUCCESS;
  }
  else if (ch == L'\'') {
    int error = tryTokenInit(T_QUOTE, L"'", s->position, 1, token);
    if (error != LEX_SUCCESS) {
      return LEX_ERROR;
    }
    return LEX_SUCCESS;
  }

  // multi-character tokens
  if (iswdigit(ch)) {
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
    if (DEBUG) { printf("error: unrecognized token '%lc'\n", ch); };
    return LEX_ERROR;
  }
}

typedef struct TokenStream {
  FILE *file;
  LexerState *lexer;
  Token* next;
} TokenStream;

int tryStreamMakeFile(char *filename, TokenStream **ptr) {

  FILE *file;
  TokenStream *s;

  file = fopen(filename, "r");
  if (file == NULL) {
    if (DEBUG) { printf("error: making stream %s\n", strerror(errno)); }
    goto error;
  }

  int error = tryStreamMake(file, &s);
  if (error != LEX_SUCCESS) {
    goto error;
  }

  *ptr = s;
  return LEX_SUCCESS;

  error:
    if (file != NULL && fclose(file) != 0) {
      if (DEBUG) {
        char *errorString = strerror(errno);
        printf("error: closing stream %s\n", errorString);
      }
    }
    tryStreamFree(s);
    return LEX_ERROR;
}

// TODO: update all docs on errors returned
int tryStreamMake(FILE *file, TokenStream **ptr) {

  LexerState *l;
  TokenStream *s;

  if (tryLexerStateMake(&l)) {
    if (DEBUG) { printf("error: malloc-ing LexerState\n"); }
    goto error;
  }

  s = malloc(sizeof(TokenStream));
  if (s == NULL) {
    if (DEBUG) { printf("error: malloc-ing TokenStream\n"); }
    goto error;
  }
  s->file = file;
  s->lexer = l;
  s->next = NULL;

  *ptr = s;
  return LEX_SUCCESS;

  error:
  lexerStateFree(l);
  free(s);
  return LEX_ERROR;
}

int tryStreamNext(TokenStream *s, Token **ptr) {

  if (s->next != NULL) {
    *ptr = s->next;
    s->next = NULL;
    return LEX_SUCCESS;
  }

  Token *t;
  int read = tryTokenRead(s->file, s->lexer, &t);

  if (read == LEX_ERROR) {
    free(t);
  }
  else {
    *ptr = t;
  }

  return read;
}

int tryStreamPeek(TokenStream *s, Token **ptr) {

  if (s->next != NULL) {
    *ptr = s->next;
    s->next = NULL;
    return LEX_SUCCESS;
  }

  Token *t = malloc(sizeof(Token));
  if (t) {
    if (DEBUG) { printf("error: malloc-ing Token\n"); }
    return LEX_ERROR;
  }

  int read = tryTokenRead(s->file, s->lexer, &t);

  if (read == LEX_ERROR) {
    free(t);
  }
  else {
    s->next = t;
    *ptr = t;
  }

  return read;
}

int tryStreamFree(TokenStream *s) {
  if (s == NULL) {
    return LEX_SUCCESS;
  }

  int closeError = 0;
  if (s->file != NULL && fclose(s->file) != 0) {
    if (DEBUG) {
      char *errorString = strerror(errno);
      printf("error closing stream: %s\n", errorString);
    }
    closeError = 1;
  }

  lexerStateFree(s->lexer);
  tokenFree(s->next);
  free(s);

  return closeError;
}

