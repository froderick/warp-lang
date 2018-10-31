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
 * Auto-expanding string buffer implementation.
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
    return LEX_ERROR;
  }

  b->usedChars = 0;
  b->allocatedChars = 256;

  if (NULL == (data = malloc(bufferAllocatedBytes(b)))) {
    if (DEBUG) { printf("error: malloc-ing StringBuffer array\n"); }
    free(b);
    return LEX_ERROR;
  }

  bzero(data, bufferAllocatedBytes(b));

  b->data = data;
  *ptr = b;
  return LEX_SUCCESS;
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

    b->data = realloc(b->data, newSizeInBytes);
    if (b->data == NULL) {
      if (DEBUG) { printf("error: realloc-ing StringBuffer array\n"); }
      return LEX_ERROR;
    }

    b->allocatedChars = b->allocatedChars * 2;
  }

  b->data[b->usedChars] = ch;
  b->usedChars = b->usedChars + 1;
  b->data[b->usedChars] = L'\0';

  return LEX_SUCCESS;
}

void bufferClear(StringBuffer *b) {
  bzero(b->data, bufferUnusedBytes(b));
  b->usedChars = 0;
}

/*
 * Token Model and Lexer
 */

typedef struct LexerState {
  unsigned long position;
  StringBuffer *b;
} LexerState;

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

int tryTokenInitFromLexer(LexerState *s, TokenType type, Token **ptr) {
  return tryTokenInit(type, s->b->data, s->position, s->b->usedChars, ptr);
}

void tokenFree(Token *t) {
  free(t);
}

int tryLexerStateMake(LexerState **ptr) {

  StringBuffer *b = NULL;
  LexerState *s = NULL;

  if (tryBufferMake(&b) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  s = malloc(sizeof(LexerState));
  if (s == NULL) {
    if (DEBUG) { printf("error: malloc-ing LexerState\n"); }
    bufferFree(b);
    return LEX_ERROR;
  }

  s->position = 0;
  s->b = b;

  *ptr = s;
  return LEX_SUCCESS;
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
  return LEX_SUCCESS;
}

int tryUnreadChar(FILE *stream, LexerState *s, wchar_t ch) {
  wint_t result = ungetwc(ch, stream);
  if (result == WEOF && s->b->usedChars == 0) {
    if (DEBUG) { printf("error: failed to push character back onto stream"); }
    return LEX_ERROR;
  }
  return LEX_SUCCESS;
}

int tryReadNumber(FILE *stream, LexerState *s, wchar_t first, Token **token) {

  if (tryBufferAppend(s->b, first) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is not numeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = tryReadChar(stream, s, &ch);
    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      matched = false;
    }
    else if (!iswdigit(ch)) {
      if (tryUnreadChar(stream, s, ch) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (tryTokenInitFromLexer(s, T_NUMBER, token) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

bool tryReadSymbol(FILE *stream, LexerState *s, wchar_t first, Token **token) {

  if (tryBufferAppend(s->b, first) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = tryReadChar(stream, s, &ch);
    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (tryUnreadChar(stream, s, ch) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = true;
    }

  } while (matched);

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

  if (tryTokenInitFromLexer(s, type, token) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

bool tryReadKeyword(FILE *stream, LexerState *s, wchar_t first, Token **token) {

  if (tryBufferAppend(s->b, first) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = tryReadChar(stream, s, &ch);
    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (tryUnreadChar(stream, s, ch) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (s->b->usedChars == 0) {
    if (DEBUG) { printf("error: keyword token type cannot be empty"); }
    return LEX_ERROR;
  }

  if (tryTokenInitFromLexer(s, T_KEYWORD, token) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

int tryReadString(FILE *stream, LexerState *s, wchar_t first, Token **token) {

  if (tryBufferAppend(s->b, first) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is a non-escaped quote

  bool foundEnd = false;
  bool escape = false;
  wint_t ch;
  do {

    int read = tryReadChar(stream, s, &ch);
    if (read != LEX_SUCCESS) {
      return read;
    }

    if (tryBufferAppend(s->b, ch) != LEX_SUCCESS) {
      return LEX_ERROR;
    }

    if (!escape && ch == L'"') {
      foundEnd = true;
    }
    else {
      escape = !escape && ch == L'\\';
    }
  }
  while (!foundEnd);

  if (tryTokenInitFromLexer(s, T_STRING, token) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  return LEX_SUCCESS;
}

int tryTokenRead(FILE *stream, LexerState *s, Token **token) {

  bufferClear(s->b);

  wint_t ch;

  while (true) {
    int read = tryReadChar(stream, s, &ch);
    if (read != LEX_SUCCESS) {
      return read;
    }
    if (isWhitespace(ch)) {
      s->position = s->position + 1;
    }
    else {
      break;
    }
  }

  int ret;

  // single-character tokens, do not require buffering
  if (ch == L'(') {
    ret = tryTokenInit(T_OPAREN, L"(", s->position, 1, token);
  }
  else if (ch == L')') {
    ret = tryTokenInit(T_CPAREN, L")", s->position, 1, token);
  }
  else if (ch == L'[') {
    ret = tryTokenInit(T_OVEC, L"[", s->position, 1, token);
  }
  else if (ch == L']') {
    ret = tryTokenInit(T_CVEC, L"]", s->position, 1, token);
  }
  else if (ch == L'{') {
    ret = tryTokenInit(T_OBRACKET, L"{", s->position, 1, token);
  }
  else if (ch == L'}') {
    ret = tryTokenInit(T_CBRACKET, L"}", s->position, 1, token);
  }
  else if (ch == L'\'') {
    ret = tryTokenInit(T_QUOTE, L"'", s->position, 1, token);
  }

  // multi-character tokens
  else if (iswdigit(ch)) {
    ret = tryReadNumber(stream, s, ch, token);
  }
  else if (iswalpha(ch)) {
    ret = tryReadSymbol(stream, s, ch, token);
  }
  else if (ch == L':') {
    ret = tryReadKeyword(stream, s, ch, token);
  }
  else if (ch == L'"') {
    ret = tryReadString(stream, s, ch, token);
  }

  // invalid token
  else {
    if (DEBUG) { printf("error: unrecognized token '%lc'\n", ch); };
    ret = LEX_ERROR;
  }

  // if we created a token, increment the lexer position
  if (ret != LEX_ERROR && *token != NULL) {
    s->position = s->position + (*token)->length;
  }

  return ret;
}

/*
 * A stream-based public API for the lexer. Makes it easy to iterate over
 * tokens. Lets you peek one token ahead. Allocates memory for each token,
 * which must be freed after use by calling #tokenFree().
 */

typedef struct TokenStream {
  FILE *file;
  LexerState *lexer;
  Token* next;
} TokenStream;

int tryStreamMake(FILE *file, TokenStream **ptr) {

  LexerState *l;
  TokenStream *s;

  if (tryLexerStateMake(&l)) {
    if (DEBUG) { printf("error: malloc-ing LexerState\n"); }
    return LEX_ERROR;
  }

  s = malloc(sizeof(TokenStream));
  if (s == NULL) {
    if (DEBUG) { printf("error: malloc-ing TokenStream\n"); }
    lexerStateFree(l);
    return LEX_ERROR;
  }

  s->file = file;
  s->lexer = l;
  s->next = NULL;

  *ptr = s;
  return LEX_SUCCESS;
}

int tryStreamMakeFile(char *filename, TokenStream **ptr) {

  FILE *file;
  TokenStream *s;

  file = fopen(filename, "r");
  if (file == NULL) {
    if (DEBUG) { printf("error: making stream %s\n", strerror(errno)); }
    return LEX_ERROR;
  }

  if (tryStreamMake(file, &s) != LEX_SUCCESS) {
    if (!fclose(file)) {
      if (DEBUG) { printf("error: closing stream %s\n", strerror(errno)); }
    }
    return LEX_ERROR;
  }

  *ptr = s;
  return LEX_SUCCESS;
}

int tryStreamNext(TokenStream *s, Token **ptr) {

  *ptr = NULL; // clear this so folks can free it after this call without risk of a double-free

  if (s->next != NULL) {
    *ptr = s->next;
    s->next = NULL;
    return LEX_SUCCESS;
  }

  return tryTokenRead(s->file, s->lexer, ptr);
}

int tryStreamPeek(TokenStream *s, Token **ptr) {

  if (s->next != NULL) {
    *ptr = s->next;
    s->next = NULL;
    return LEX_SUCCESS;
  }

  int read = tryTokenRead(s->file, s->lexer, ptr);

  if (read != LEX_ERROR) {
    s->next = *ptr;
  }

  return read;
}

int tryStreamFree(TokenStream *s) {

  if (s == NULL) {
    return LEX_SUCCESS;
  }

  int closeError = LEX_SUCCESS;
  if (s->file != NULL && fclose(s->file)) {
    if (DEBUG) { printf("error: closing stream on free -> '%s'\n", strerror(errno)); }
    closeError = LEX_ERROR;
  }

  lexerStateFree(s->lexer);
  tokenFree(s->next);
  free(s);

  return closeError;
}

