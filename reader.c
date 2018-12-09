#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>
#include <errno.h>

#include "utils.h"
#include "reader.h"

/*
 * Lexer input source abstraction.
 *
 * Mainly did this because not all operating systems have the builtin concept
 * of a 'string stream' (looking at you, MacOS).
 */

typedef struct StreamSource {
  void *state;
  RetVal (*readChar)(void *state, wchar_t *ch, Error *error);
  RetVal (*unreadChar)(void * state, wchar_t ch, Error *error);
  RetVal (*freeState)(void *state, Error *error);
} StreamSource;

RetVal trySourceMake(
    void *state,
    RetVal (*readChar)(void *state, wchar_t *ch, Error *error),
    RetVal (*unreadChar)(void * state, wchar_t ch, Error *error),
    RetVal (*freeState)(void *state, Error *error),
    StreamSource **ptr,
    Error *error) {

  StreamSource *s;

  if (NULL == (s = malloc(sizeof(StreamSource)))) {
    return memoryError(error, "malloc StreamSource");
  }

  s->state = state;
  s->readChar = readChar;
  s->unreadChar = unreadChar;
  s->freeState = freeState;

  *ptr = s;
  return R_SUCCESS;
}

RetVal tryReadCharFromFILE(void *state, wchar_t* ch, Error *error) {

  FILE *stream = (FILE*)state;

  *ch = fgetwc(stream);
  if (*ch == WEOF) {
    if (feof(stream)) {
      return R_EOF;
    }
    else {
      return ioError(error, "read token from stream");
    }
  }
  return R_SUCCESS;
}

RetVal tryUnreadCharToFILE(void *state, wchar_t ch, Error *error) {

  FILE *stream = (FILE*)state;

  wint_t result = ungetwc(ch, stream);
  if (result == WEOF) {
    return ioError(error, "push character back onto stream");
  }
  return R_SUCCESS;
}

RetVal tryFreeFILE(void *state, Error *error) {

  FILE *stream = (FILE*)state;

  if (stream != NULL && fclose(stream)) {
    return ioError(error, "closing stream on free");
  }
  return R_SUCCESS;
}

RetVal trySourceMakeFile(FILE *file, StreamSource **s, Error *error) {
  return trySourceMake(
      file,
      tryReadCharFromFILE,
      tryUnreadCharToFILE,
      tryFreeFILE,
      s,
      error);
}

RetVal trySourceMakeFilename(char *filename, StreamSource **s, Error *error) {

  FILE *file;

  file = fopen(filename, "r");
  if (file == NULL) {
    return ioError(error, "making stream from file");
  }

  if (trySourceMakeFile(file, s, error) != R_SUCCESS) {
    if (!fclose(file)) {
      return ioError(error, "closing file stream");
    }
    return R_ERROR;
  }

  return R_SUCCESS;
}

typedef struct StringStream {
  wchar_t* text;
  uint64_t length;
  uint64_t next;
} StringStream;

RetVal tryReadCharFromString(void *state, wchar_t* ch, Error *error) {

  StringStream *stream = (StringStream*)state;

  if (stream->next == stream->length) {
    return R_EOF;
  }

  *ch = stream->text[stream->next];
  stream->next = stream->next + 1;
  return R_SUCCESS;
}

RetVal tryUnreadCharToString(void *state, wchar_t ch, Error *error) {

  StringStream *stream = (StringStream*)state;

  if (stream->next > 0) {
    stream->next = stream->next - 1;
  }

  return R_SUCCESS;
}

RetVal tryFreeString(void *state, Error *error) {
  StringStream *stream = (StringStream*)state;
  free(stream);
  return R_SUCCESS;
}

RetVal trySourceMakeString(wchar_t* text, uint64_t length, StreamSource_t *s, Error *error) {

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

RetVal trySourceReadChar(StreamSource *source, wchar_t* ch, Error *error) {
  return source->readChar(source->state, ch, error);
}

RetVal trySourceUnreadChar(StreamSource *source, wchar_t ch, Error *error) {
  return source->unreadChar(source->state, ch, error);
}

RetVal trySourceFree(StreamSource *s, Error *error) {

  int freeSuccess = R_SUCCESS;
  if (s->freeState != NULL) {
    freeSuccess = s->freeState(s->state, error);
  }
  free(s);

  if (freeSuccess != R_SUCCESS) {
    return R_ERROR;
  }

  return R_SUCCESS;
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

RetVal tryBufferMake(StringBuffer **ptr, Error *error) {

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
  return R_SUCCESS;
}

void bufferFree(StringBuffer *b) {
  if (b != NULL) {
    free(b->data);
  }
  free(b);
}

RetVal tryBufferAppend(StringBuffer *b, wchar_t ch, Error *error) {

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

  return R_SUCCESS;
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
  unsigned long lineNumber;
  unsigned long colNumber;
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

RetVal tryTokenInit(TokenType type, wchar_t *text, unsigned long position, unsigned long length,
                    unsigned long lineNumber, unsigned long colNumber, Token **ptr, Error *error) {

  Token *t;

  t = malloc(sizeof(Token) + (sizeof(wchar_t) * length) + 1);
  if (t == NULL) {
    return memoryError(error, "malloc Token");
  }

  t->type = type;
  t->typeName = tokenName(type);
  t->source.isSet = true;
  t->source.position = position;
  t->source.length = length;
  t->source.lineNumber = lineNumber;
  t->source.colNumber = colNumber;
  wcpncpy(t->text, text, length);
  t->text[length] = L'\0';

  *ptr = t;

  return R_SUCCESS;
}

RetVal tryTokenInitFromLexer(LexerState *s, TokenType type, Token **ptr, Error *error) {
  return tryTokenInit(type, s->b->data, s->position, s->b->usedChars, s->lineNumber, s->colNumber, ptr, error);
}

void tokenFree(Token *t) {
  free(t);
}

RetVal tryLexerStateMake(LexerState **ptr, Error *error) {

  StringBuffer *b = NULL;
  LexerState *s = NULL;

  if (tryBufferMake(&b, error) != R_SUCCESS) {
    return R_ERROR;
  }

  s = malloc(sizeof(LexerState));
  if (s == NULL) {
    bufferFree(b);
    return memoryError(error, "malloc LexerState");
  }

  s->position = 0;
  s->lineNumber = 1;
  s->colNumber = 1;
  s->b = b;

  *ptr = s;
  return R_SUCCESS;
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

bool isNewline(wchar_t ch) {
  return ch == L'\n';
}

RetVal tryReadString(StreamSource *source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is a non-escaped quote

  bool foundEnd = false;
  bool escape = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      return tokenizationError(error, s->position, "unexpected EOF, string must end in a '\"''");
    }

    if (tryBufferAppend(s->b, ch, error) != R_SUCCESS) {
      return R_ERROR;
    }

    if (!escape && ch == L'"') {
      foundEnd = true;
    }
    else {
      escape = !escape && ch == L'\\';
    }
  }
  while (!foundEnd);

  if (tryTokenInitFromLexer(s, T_STRING, token, error) != R_SUCCESS) {
    return R_ERROR;
  }

  return R_SUCCESS;
}

RetVal tryReadNumber(StreamSource *source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not numeric, then push back

  bool matched;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      matched = false;
    }
    else if (!iswdigit(ch)) {
      if (trySourceUnreadChar(source, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (tryTokenInitFromLexer(s, T_NUMBER, token, error) != R_SUCCESS) {
    return R_ERROR;
  }

  return R_SUCCESS;
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

RetVal tryReadSymbol(StreamSource *source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back
  // on EOF, stop reading and return R_SUCCESS

  bool matched;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (trySourceUnreadChar(source, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch, error) != R_SUCCESS) {
        return R_ERROR;
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

  if (tryTokenInitFromLexer(s, type, token, error) != R_SUCCESS) {
    return R_ERROR;
  }

  return R_SUCCESS;
}

RetVal tryReadKeyword(StreamSource *source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (trySourceUnreadChar(source, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = false;
    }
    else {
      if (tryBufferAppend(s->b, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (s->b->usedChars == 1) {
    return tokenizationError(error, s->position, "keyword token type cannot be empty");
  }

  if (tryTokenInitFromLexer(s, T_KEYWORD, token, error) != R_SUCCESS) {
    return R_ERROR;
  }

  return R_SUCCESS;
}

RetVal tryTokenRead(StreamSource *source, LexerState *s, Token **token, Error *error) {

  bufferClear(s->b);

  wint_t ch;

  while (true) {
    int read = trySourceReadChar(source, &ch, error);
    if (read != R_SUCCESS) {
      return read;
    }
    if (isWhitespace(ch)) {
      s->position = s->position + 1;

      if (isNewline(ch)) {
        s->lineNumber = s->lineNumber + 1; // newlines increment lineNumber
        s->colNumber = 1;                  // newlines reset colNumber
      }
    }
    else {
      break;
    }
  }

  int ret;

  // single-character tokens, do not require buffering
  if (ch == L'(') {
    ret = tryTokenInit(T_OPAREN, L"(", s->position, 1, s->lineNumber, s->colNumber, token, error);
  }
  else if (ch == L')') {
    ret = tryTokenInit(T_CPAREN, L")", s->position, 1, s->lineNumber, s->colNumber, token, error);
  }
//  else if (ch == L'[') {
//    ret = tryTokenInit(T_OVEC, L"[", s->position, 1, token, error);
//  }
//  else if (ch == L']') {
//    ret = tryTokenInit(T_CVEC, L"]", s->position, 1, token, error);
//  }
//  else if (ch == L'{') {
//    ret = tryTokenInit(T_OBRACKET, L"{", s->position, 1, token, error);
//  }
//  else if (ch == L'}') {
//    ret = tryTokenInit(T_CBRACKET, L"}", s->position, 1, token, error);
//  }
  else if (ch == L'\'') {
    ret = tryTokenInit(T_QUOTE, L"'", s->position, 1, s->lineNumber, s->colNumber, token, error);
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
  if (ret != R_ERROR && *token != NULL) {
    s->position = s->position + (*token)->source.length;
    s->colNumber = s->colNumber + (*token)->source.length;
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

RetVal tryStreamMake(StreamSource *source, TokenStream **ptr, Error *error) {

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
  return R_SUCCESS;
}

RetVal tryStreamMakeFile(char *filename, TokenStream **ptr, Error *error) {

  FILE *file;
  TokenStream *s;

  file = fopen(filename, "r");
  if (file == NULL) {
    return ioError(error, "making stream from file");
  }

  if (tryStreamMake((void*)file, &s, error) != R_SUCCESS) {
    if (!fclose(file)) {
      return ioError(error, "closing file stream");
    }
    return R_ERROR;
  }

  *ptr = s;
  return R_SUCCESS;
}

RetVal tryStreamNext(TokenStream *s, Token **token, Error *error) {

  // clear this so folks can free it after this call without risk of a double-free
  *token = NULL;

  if (s->next != NULL) { // eat the cached next token if we have one
    *token = s->next;
    s->next = NULL;
    return R_SUCCESS;
  }

  return tryTokenRead(s->source, s->lexer, token, error);
}

RetVal tryStreamPeek(TokenStream *s, Token **token, Error *error) {

  if (s->next != NULL) { // return the cached next token if we have one
    *token = s->next;
    return R_SUCCESS;
  }

  int read = tryTokenRead(s->source, s->lexer, token, error);

  if (read != R_ERROR) {
    s->next = *token;
  }

  return read;
}

void streamDropPeeked(TokenStream *s) {
  s->next = NULL;
}

// this frees the source as well
RetVal tryStreamFree(TokenStream *s, Error *error) {

  if (s == NULL) {
    return R_SUCCESS;
  }

  int closeError = trySourceFree(s->source, error);

  lexerStateFree(s->lexer);
  tokenFree(s->next);
  free(s);

  return closeError;
}

/*
 * Here is the basic AST implementation.
 */

RetVal tryStringMake(wchar_t *input, uint64_t length, Expr **ptr, Error *error) {

  RetVal ret;
  wchar_t *text;

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  ret = tryCopyText(input, &text, length, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

RetVal tryStringRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_STRING) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_STRING");
    goto failure;
  }

  if (token->source.length < 2) {
    ret = syntaxError(error, token->source.position, "Token should start and end with '\"'");
    goto failure;
  }

  // trim quotes
  wchar_t *text = token->text + 1;
  uint64_t len = token->source.length - 2;

  Expr *expr;
  ret = tryStringMake(text, len, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_NUMBER;
  expr->number.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryNumberRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_NUMBER) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_NUMBER");
    goto failure;
  }

  uint64_t value = wcstoull(token->text, NULL, 0);
  if (errno == EINVAL) {
    ret = syntaxError(error, token->source.position, "Token text does not represent a valid number");
    goto failure;
  }
  if (errno == ERANGE) {
    ret = syntaxError(error, token->source.position, "Cannot represent a number literal larger than 64 bits unsigned");
    goto failure;
  }

  Expr *expr;
  ret = tryNumberMake(value, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  ret = tryCopyText(name, &value, len, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

RetVal trySymbolRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_SYMBOL) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_SYMBOL");
    goto failure;
  }

  Expr *expr;
  ret = trySymbolMake(token->text, token->source.length, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

  ret = tryCopyText(name, &text, len, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

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

RetVal tryKeywordRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_KEYWORD) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_KEYWORD");
    goto failure;
  }

  uint64_t len = token->source.length - 1;
  wchar_t *text = token->text + 1;

  Expr *expr;
  ret = tryKeywordMake(text, len, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_BOOLEAN;
  expr->boolean.value = value;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryBooleanRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_TRUE && token->type != T_FALSE) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_TRUE or T_FALSE");
    goto failure;
  }

  bool value = token->type == T_TRUE;

  Expr *expr;
  ret = tryBooleanMake(value, &expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_NIL;
  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryNilRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_NIL) {
    ret = syntaxError(error, token->source.position, "Token is not a type of T_NIL");
    goto failure;
  }

  Expr *expr;
  ret = tryNilMake(&expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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

RetVal tryExprRead(TokenStream *stream, Expr **ptr, Error *error);
RetVal tryListAppend(ExprList *list, Expr *expr, Error *error);

// Assume the opening paren has aready been read.
// Allocate a list, continue to read expressions and add them to it until a
// closed-paren is found.

RetVal tryListMake(Expr **ptr, Error *error) {

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    return memoryError(error, "malloc Expr");
  }

  expr->type = N_LIST;
  expr->list.length = 0;

  // valid for zero length list
  expr->list.head = NULL;
  expr->list.tail = NULL;

  expr->source.isSet = false;

  *ptr = expr;
  return R_SUCCESS;
}

RetVal tryListRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;

  // these get cleaned up on failure
  Token *oParen, *cParen;
  Expr *expr;
  Expr *subexpr;

  // convenience

  ret = tryListMake(&expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryStreamNext(stream, &oParen, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (oParen->type != T_OPAREN) {
    ret = syntaxError(error, oParen->source.position, "List must begin with ')'");
    goto failure;
  }

  while (true) {

    Token *cParen;
    ret = tryStreamPeek(stream, &cParen, error);
    if (ret != R_SUCCESS) {
      if (ret == R_EOF) { // eof too soon
        ret = syntaxError(error, oParen->source.position, "Encountered EOF, was expecting ')'");
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
      return R_SUCCESS;
    }
    else { // read a new expression and add it to the list

      ret = tryExprRead(stream, &subexpr, error);
      if (ret != R_SUCCESS) {
        goto failure;
      }

      ret = tryListAppend(&expr->list, subexpr, error);
      if (ret != R_SUCCESS) {
        goto failure;
      }
      subexpr = NULL; // subexpr is part of list now
    }
  }

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
    exprFree(expr);
    return ret;
}

RetVal tryListAppend(ExprList *list, Expr *expr, Error *error) {

  ListElement *elem =  malloc(sizeof(ListElement));
  if (elem == NULL) {
    return memoryError(error, "malloc ExprList");
  }

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
}

void listFreeContents(ExprList *list) {
  while (list->head != NULL) {
    ListElement *elem = list->head;
    list->head = list->head->next;
    exprFree(elem->expr);
    free(elem);
  }
}

RetVal tryQuoteRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;

  Token *token;
  Expr *quote;
  Expr *subexpr;
  Expr *expr;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_QUOTE) {
    ret = syntaxError(error, token->source.position, "Syntax quote must begin with '`'");
    goto failure;
  }

  ret = trySymbolMake(L"quote", wcslen(L"quote"), &quote, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
  quote->source = token->source;
  token = NULL; // token is now a part of quote symbol

  ret = tryExprRead(stream, &subexpr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryListMake(&expr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryListAppend(&expr->list, quote, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
  quote = NULL; // quote is now part of expr

  ret = tryListAppend(&expr->list, subexpr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
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

RetVal tryExprRead(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *peek;

  ret = tryStreamPeek(stream, &peek, error);
  if (ret != R_SUCCESS) {
    return ret; // can be R_EOF or R_ERROR
  }

  switch (peek->type) {

    // atoms
    case T_STRING:
      ret = tryStringRead(stream, ptr, error);
      break;
    case T_NUMBER:
      ret = tryNumberRead(stream, ptr, error);
      break;
    case T_SYMBOL:
      ret = trySymbolRead(stream, ptr, error);
      break;
    case T_KEYWORD:
      ret = tryKeywordRead(stream, ptr, error);
      break;
    case T_TRUE:
    case T_FALSE:
      ret = tryBooleanRead(stream, ptr, error);
      break;
    case T_NIL:
      ret = tryNilRead(stream, ptr, error);
      break;

    // lists
    case T_OPAREN:
      ret = tryListRead(stream, ptr, error);
      break;

    // reader macros
    case T_QUOTE:
      ret = tryQuoteRead(stream, ptr, error);
      break;

    default:
      ret = syntaxError(error, peek->source.position, "Unknown token type");
  }

  if (ret != R_SUCCESS) {
    return ret;
  }

  return R_SUCCESS;
}

void exprFree(Expr *expr) {
  if (expr->type == N_STRING) {
    stringFreeContents(&expr->string);
  }
  else if (expr->type == N_NUMBER) {
    // nothing to do
  }
  else if (expr->type == N_SYMBOL) {
    symbolFreeContents(&expr->symbol);
  }
  else if (expr->type == N_KEYWORD) {
    keywordFreeContents(&expr->keyword);
  }
  else if (expr->type == N_BOOLEAN) {
    // nothing to do
  }
  else if (expr->type == N_NIL) {
    // nothing to do
  }
  else if (expr->type == N_LIST) {
    listFreeContents(&expr->list);
  }
  free(expr);
}

RetVal tryDeepCopy(Expr *from, Expr **ptr, Error *error) {

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

        throws(tryDeepCopy(elem->expr, &listItem, error));
        throws(tryListAppend(&to->list, listItem, error));
        listItem = NULL; // item is now part of list

        elem = elem->next;
      }
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






















