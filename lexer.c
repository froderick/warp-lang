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
 * Error factories
 */

int memoryError(LexerError *error, char *desc) {

  error->type = LE_MEMORY;
  error->position = 0;

  swprintf(error->message, LEX_ERROR_MSG_LENGTH, L"failed to %s\n", desc);

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return LEX_ERROR;
}

int ioError(LexerError *error, char *desc) {

  error->type = LE_IO;
  error->position = 0;
  swprintf(error->message, LEX_ERROR_MSG_LENGTH, L"failed to %s ->  '%s'\n", desc, strerror(errno));

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return LEX_ERROR;
}

int tokenizationError(LexerError *error, unsigned long position, char *desc) {

  error->type = LE_TOKENIZATION;
  error->position = position;
  swprintf(error->message, LEX_ERROR_MSG_LENGTH, L"failed to tokenize stream -> %s\n", desc);

  if (DEBUG) { printf("error: %ls\n", error->message); }
  return LEX_ERROR;
}

/*
 * Lexer input source abstraction.
 *
 * Mainly did this because not all operating systems have the builtin concept
 * of a 'string stream' (looking at you, MacOS).
 */

typedef struct StreamSource {
  void *state;
  int (*readChar)(void *state, wchar_t *ch, LexerError *error);
  int (*unreadChar)(void * state, wchar_t ch, LexerError *error);
  int (*freeState)(void *state, LexerError *error);
} StreamSource;

int trySourceMake(
    void *state,
    int (*readChar)(void *state, wchar_t *ch, LexerError *error),
    int (*unreadChar)(void * state, wchar_t ch, LexerError *error),
    int (*freeState)(void *state, LexerError *error),
    StreamSource **ptr,
    LexerError *error) {

  StreamSource *s;

  if (NULL == (s = malloc(sizeof(StreamSource)))) {
    return memoryError(error, "malloc StreamSource");
  }

  s->state = state;
  s->readChar = readChar;
  s->unreadChar = unreadChar;
  s->freeState = freeState;

  *ptr = s;
  return LEX_SUCCESS;
}

int tryReadCharFromFILE(void *state, wchar_t* ch, LexerError *error) {

  FILE *stream = (FILE*)state;

  *ch = fgetwc(stream);
  if (*ch == WEOF) {
    if (feof(stream)) {
      return LEX_EOF;
    }
    else {
      return ioError(error, "read token from stream");
    }
  }
  return LEX_SUCCESS;
}

int tryUnreadCharToFILE(void *state, wchar_t ch, LexerError *error) {

  FILE *stream = (FILE*)state;

  wint_t result = ungetwc(ch, stream);
  if (result == WEOF) {
    return ioError(error, "push character back onto stream");
  }
  return LEX_SUCCESS;
}

int tryFreeFILE(void *state, LexerError *error) {

  FILE *stream = (FILE*)state;

  if (stream != NULL && fclose(stream)) {
    return ioError(error, "closing stream on free");
  }
  return LEX_SUCCESS;
}

int trySourceMakeFile(FILE *file, StreamSource **s, LexerError *error) {
  return trySourceMake(
      file,
      tryReadCharFromFILE,
      tryUnreadCharToFILE,
      tryFreeFILE,
      s,
      error);
}

int trySourceMakeFilename(char *filename, StreamSource **s, LexerError *error) {

  FILE *file;

  file = fopen(filename, "r");
  if (file == NULL) {
    return ioError(error, "making stream from file");
  }

  if (trySourceMakeFile(file, s, error) != LEX_SUCCESS) {
    if (!fclose(file)) {
      return ioError(error, "closing file stream");
    }
    return LEX_ERROR;
  }

  return LEX_SUCCESS;
}

typedef struct StringStream {
  wchar_t* text;
  uint64_t length;
  uint64_t next;
} StringStream;

int tryReadCharFromString(void *state, wchar_t* ch, LexerError *error) {

  StringStream *stream = (StringStream*)state;

  if (stream->next == stream->length) {
    return LEX_EOF;
  }

  *ch = stream->text[stream->next];
  stream->next = stream->next + 1;
  return LEX_SUCCESS;
}

int tryUnreadCharToString(void *state, wchar_t ch, LexerError *error) {

  StringStream *stream = (StringStream*)state;

  if (stream->next > 0) {
    stream->next = stream->next - 1;
  }

  return LEX_SUCCESS;
}

int tryFreeString(void *state, LexerError *error) {
  StringStream *stream = (StringStream*)state;
  free(stream);
  return LEX_SUCCESS;
}

int trySourceMakeString(wchar_t* text, uint64_t length, StreamSource_t *s, LexerError *error) {

  StringStream *state;

  if (NULL == (state = malloc(sizeof(StringStream)))) {
    return memoryError(error, "malloc StringStream");
  }

  state->text = text;
  state->length = length;
  state->next = 0;

  return trySourceMake(
      (void*)state,
      tryReadCharFromString,
      tryUnreadCharToString,
      tryFreeString,
      s,
      error);
}

int trySourceReadChar(StreamSource *source, wchar_t* ch, LexerError *error) {
  return source->readChar(source->state, ch, error);
}

int trySourceUnreadChar(StreamSource *source, wchar_t ch, LexerError *error) {
  return source->unreadChar(source->state, ch, error);
}

int trySourceFree(StreamSource *s, LexerError *error) {

  int freeSuccess = LEX_SUCCESS;
  if (s->freeState != NULL) {
    freeSuccess = s->freeState(s->state, error);
  }
  free(s);

  if (freeSuccess != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  return LEX_SUCCESS;
}

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

int tryBufferMake(StringBuffer **ptr, LexerError *error) {

  StringBuffer *b;
  wchar_t *data;

  if (NULL == (b = malloc(sizeof(StringBuffer)))) {
    return memoryError(error, "malloc StringBuffer");
  }

  b->usedChars = 0;
  b->allocatedChars = 256;

  if (NULL == (data = malloc(bufferAllocatedBytes(b)))) {
    free(b);
    return memoryError(error, "malloc StringBuffer array");
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

int tryBufferAppend(StringBuffer *b, wchar_t ch, LexerError *error) {

  if (b->usedChars + 1 == (b->allocatedChars - 1)) {

    unsigned long oldSizeInBytes = bufferAllocatedBytes(b);
    unsigned long newSizeInBytes = oldSizeInBytes * 2;

    b->data = realloc(b->data, newSizeInBytes);
    if (b->data == NULL) {
      return memoryError(error, "realloc StringBuffer array");
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

int tryTokenInit(TokenType type, wchar_t *text, unsigned long position, unsigned long length,
                 Token **ptr, LexerError *error) {

  Token *t;

  t = malloc(sizeof(Token) + (sizeof(wchar_t) * length) + 1);
  if (t == NULL) {
    return memoryError(error, "malloc Token");
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

int tryTokenInitFromLexer(LexerState *s, TokenType type, Token **ptr, LexerError *error) {
  return tryTokenInit(type, s->b->data, s->position, s->b->usedChars, ptr, error);
}

void tokenFree(Token *t) {
  free(t);
}

int tryLexerStateMake(LexerState **ptr, LexerError *error) {

  StringBuffer *b = NULL;
  LexerState *s = NULL;

  if (tryBufferMake(&b, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  s = malloc(sizeof(LexerState));
  if (s == NULL) {
    bufferFree(b);
    return memoryError(error, "malloc LexerState");
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

int tryReadNumber(StreamSource *source, LexerState *s, wchar_t first, Token **token, LexerError *error) {

  if (tryBufferAppend(s->b, first, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is not numeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      matched = false;
    }
    else if (!iswdigit(ch)) {
      if (trySourceUnreadChar(source, ch, error) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch, error) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (tryTokenInitFromLexer(s, T_NUMBER, token, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

int tryReadSymbol(StreamSource *source, LexerState *s, wchar_t first, Token **token, LexerError *error) {

  if (tryBufferAppend(s->b, first, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (trySourceUnreadChar(source, ch, error) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch, error) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = true;
    }

  } while (matched);

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

  if (tryTokenInitFromLexer(s, type, token, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

int tryReadKeyword(StreamSource *source, LexerState *s, wchar_t first, Token **token, LexerError *error) {

  if (tryBufferAppend(s->b, first, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == LEX_ERROR) {
      return LEX_ERROR;
    }

    if (read == LEX_EOF) {
      eof = true;
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (trySourceUnreadChar(source, ch, error) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch, error) != LEX_SUCCESS) {
        return LEX_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (s->b->usedChars == 1) {
    return tokenizationError(error, s->position, "keyword token type cannot be empty");
  }

  if (tryTokenInitFromLexer(s, T_KEYWORD, token, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  if (eof) {
    return LEX_EOF;
  }
  else {
    return LEX_SUCCESS;
  }
}

int tryReadString(StreamSource *source, LexerState *s, wchar_t first, Token **token, LexerError *error) {

  if (tryBufferAppend(s->b, first, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  // keep reading until char is a non-escaped quote

  bool foundEnd = false;
  bool escape = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read != LEX_SUCCESS) {
      return read;
    }

    if (tryBufferAppend(s->b, ch, error) != LEX_SUCCESS) {
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

  if (tryTokenInitFromLexer(s, T_STRING, token, error) != LEX_SUCCESS) {
    return LEX_ERROR;
  }

  return LEX_SUCCESS;
}

int tryTokenRead(StreamSource *source, LexerState *s, Token **token, LexerError *error) {

  bufferClear(s->b);

  wint_t ch;

  while (true) {
    int read = trySourceReadChar(source, &ch, error);
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
    ret = tryTokenInit(T_OPAREN, L"(", s->position, 1, token, error);
  }
  else if (ch == L')') {
    ret = tryTokenInit(T_CPAREN, L")", s->position, 1, token, error);
  }
  else if (ch == L'[') {
    ret = tryTokenInit(T_OVEC, L"[", s->position, 1, token, error);
  }
  else if (ch == L']') {
    ret = tryTokenInit(T_CVEC, L"]", s->position, 1, token, error);
  }
  else if (ch == L'{') {
    ret = tryTokenInit(T_OBRACKET, L"{", s->position, 1, token, error);
  }
  else if (ch == L'}') {
    ret = tryTokenInit(T_CBRACKET, L"}", s->position, 1, token, error);
  }
  else if (ch == L'\'') {
    ret = tryTokenInit(T_QUOTE, L"'", s->position, 1, token, error);
  }

  // multi-character tokens
  else if (iswdigit(ch)) {
    ret = tryReadNumber(source, s, ch, token, error);
  }
  else if (iswalpha(ch)) {
    ret = tryReadSymbol(source, s, ch, token, error);
  }
  else if (ch == L':') {
    ret = tryReadKeyword(source, s, ch, token, error);
  }
  else if (ch == L'"') {
    ret = tryReadString(source, s, ch, token, error);
  }

  // invalid token
  else {
    int len = 32;
    char msg[len];
    snprintf(msg, len, "unrecognized token '%lc'\n", ch);
    ret = tokenizationError(error, s->position, "keyword token type cannot be empty");
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
  StreamSource *source;
  LexerState *lexer;
  Token* next;
} TokenStream;

int tryStreamMake(StreamSource *source, TokenStream **ptr, LexerError *error) {

  LexerState *l;
  TokenStream *s;

  if (tryLexerStateMake(&l, error)) {
    return memoryError(error, "malloc LexerState");
  }

  s = malloc(sizeof(TokenStream));
  if (s == NULL) {
    lexerStateFree(l);
    return memoryError(error, "malloc TokenStream");
  }

  s->source = source;
  s->lexer = l;
  s->next = NULL;

  *ptr = s;
  return LEX_SUCCESS;
}

int tryStreamMakeFile(char *filename, TokenStream **ptr, LexerError *error) {

  FILE *file;
  TokenStream *s;

  file = fopen(filename, "r");
  if (file == NULL) {
    return ioError(error, "making stream from file");
  }

  if (tryStreamMake((void*)file, &s, error) != LEX_SUCCESS) {
    if (!fclose(file)) {
      return ioError(error, "closing file stream");
    }
    return LEX_ERROR;
  }

  *ptr = s;
  return LEX_SUCCESS;
}

int tryStreamNext(TokenStream *s, Token **token, LexerError *error) {

  // clear this so folks can free it after this call without risk of a double-free
  *token = NULL;

  if (s->next != NULL) {
    *token = s->next;
    s->next = NULL;
    return LEX_SUCCESS;
  }

  return tryTokenRead(s->source, s->lexer, token, error);
}

int tryStreamPeek(TokenStream *s, Token **token, LexerError *error) {

  if (s->next != NULL) {
    *token = s->next;
    s->next = NULL;
    return LEX_SUCCESS;
  }

  int read = tryTokenRead(s->source, s->lexer, token, error);

  if (read != LEX_ERROR) {
    s->next = *token;
  }

  return read;
}

int tryStreamFree(TokenStream *s, LexerError *error) {

  if (s == NULL) {
    return LEX_SUCCESS;
  }

  int closeError = trySourceFree(s->source, error);

  lexerStateFree(s->lexer);
  tokenFree(s->next);
  free(s);

  return closeError;
}

