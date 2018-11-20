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
                 Token **ptr, Error *error) {

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

  return R_SUCCESS;
}

RetVal tryTokenInitFromLexer(LexerState *s, TokenType type, Token **ptr, Error *error) {
  return tryTokenInit(type, s->b->data, s->position, s->b->usedChars, ptr, error);
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

RetVal tryReadNumber(StreamSource *source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not numeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      eof = true;
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

  if (eof) {
    return R_EOF;
  }
  else {
    return R_SUCCESS;
  }
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

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      eof = true;
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

  if (eof) {
    return R_EOF;
  }
  else {
    return R_SUCCESS;
  }
}

RetVal tryReadKeyword(StreamSource *source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  bool eof = false;
  wint_t ch;
  do {

    int read = trySourceReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      eof = true;
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

  if (eof) {
    return R_EOF;
  }
  else {
    return R_SUCCESS;
  }
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
    if (read != R_SUCCESS) {
      return read;
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
  if (ret != R_ERROR && *token != NULL) {
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

// TODO: all the symbol token types need to become expression types, move
// all the dedicated symbol parsing behavior into the AST parsing code/model

RetVal tryExprMake(TokenStream *stream, Expr **ptr, Error *error);
void exprFree(Expr *expr);
RetVal tryListAppend(ExprList *list, Expr *expr, Error *error);
void listFreeContents(ExprList *list);
RetVal trySpecialMake(ExprType type, const wchar_t *name, TokenStream *stream, Expr **ptr, Error *error);

RetVal tryNumberMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_NUMBER) {
    ret = syntaxError(error, token->position, "Token is not a type of T_NUMBER");
    goto failure;
  }

  uint64_t value = wcstoull(token->text, NULL, 0);
  if (errno == EINVAL) {
    ret = syntaxError(error, token->position, "Token text does not represent a valid number");
    goto failure;
  }
  if (errno == ERANGE) {
    ret = syntaxError(error, token->position, "Cannot represent a number literal larger than 64 bits unsigned");
    goto failure;
  }

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = N_NUMBER;
  expr->number.token = token;
  expr->number.value = value;

  *ptr = expr;
  return R_SUCCESS;

  failure:
  if (token != NULL) {
    tokenFree(token);
  }
  return ret;
}

void numberFreeContents(ExprNumber *number) {
  if (number != NULL) {
    if (number->token != NULL) {
      tokenFree(number->token);
    }
  }
}

RetVal tryStringMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;
  wchar_t *text;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_STRING) {
    ret = syntaxError(error, token->position, "Token is not a type of T_STRING");
    goto failure;
  }

  if (token->length - 2 < 0) {
    ret = syntaxError(error, token->position, "Token should start and end with '\"'");
    goto failure;
  }

  // trim quotes
  text = malloc( sizeof(wchar_t) * (token->length - 1)); // -1 rather than -2 to leave room for null at end
  wcsncpy(text, token->text + 1, token->length - 2);

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = N_STRING;
  expr->string.token = token;
  expr->string.length = token->length - 2;
  expr->string.value = text;

  *ptr = expr;
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
    if (string->token != NULL) {
      tokenFree(string->token);
    }
    if (string->value != NULL) {
      free(string->value);
    }
  }
}

RetVal trySymbolMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;
  wchar_t *text;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_SYMBOL) {
    ret = syntaxError(error, token->position, "Token is not a type of T_SYMBOL");
    goto failure;
  }

  text = malloc( sizeof(wchar_t) * token->length);
  wcscpy(token->text, text);

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = N_SYMBOL;
  expr->symbol.token = token;
  expr->symbol.length = token->length;
  expr->symbol.value = text;

  *ptr = expr;
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

void symbolFreeContents(ExprSymbol *symbol) {
  if (symbol != NULL) {
    if (symbol->token != NULL) {
      tokenFree(symbol->token);
    }
    if (symbol->value != NULL) {
      free(symbol->value);
    }
  }
}

RetVal tryKeywordMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;
  wchar_t *text;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_KEYWORD) {
    ret = syntaxError(error, token->position, "Token is not a type of T_KEYWORD");
    goto failure;
  }

  text = malloc( sizeof(wchar_t) * token->length);
  wcscpy(token->text, text);

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = N_KEYWORD;
  expr->keyword.token = token;
  expr->keyword.length = token->length;
  expr->keyword.value = text;

  *ptr = expr;
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

void keywordFreeContents(ExprKeyword *keyword) {
  if (keyword!= NULL) {
    if (keyword->token != NULL) {
      tokenFree(keyword->token);
    }
    if (keyword->value != NULL) {
      free(keyword->value);
    }
  }
}

RetVal trySpecialMake(ExprType type, const wchar_t *name, TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }
  // skip validating this token

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = type;
  expr->special.token = token;
  expr->special.value = name;

  *ptr = expr;
  return R_SUCCESS;

  failure:
  if (token != NULL) {
    tokenFree(token);
  }
  return ret;
}

void specialFreeContents(ExprSpecial *special) {
  if (special!= NULL) {
    if (special->token != NULL) {
      tokenFree(special->token);
    }
  }
}

RetVal tryBooleanMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *token;

  ret = tryStreamNext(stream, &token, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (token->type != T_TRUE && token->type != T_FALSE) {
    ret = syntaxError(error, token->position, "Token is not a type of T_TRUE or T_FALSE");
    goto failure;
  }

  Expr *expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  bool value;
  if (token->type == T_TRUE) {
    value = true;
  }
  else {
    value = false;
  }

  expr->type = N_BOOLEAN;
  expr->boolean.token = token;
  expr->boolean.value = value;

  *ptr = expr;
  return R_SUCCESS;

  failure:
  if (token != NULL) {
    tokenFree(token);
  }
  return ret;
}

void booleanFreeContents(ExprBoolean *boolean) {
  if (boolean != NULL) {
    if (boolean->token != NULL) {
      tokenFree(boolean->token);
    }
  }
}

// Lists

// Assume the opening paren has aready been read.
// Allocate a list, continue to read expressions and add them to it until a
// closed-paren is found.

RetVal tryListMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;

  ExprList list;
  Expr *subexpr;

  ret = tryStreamNext(stream, &list.oParen, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (list.oParen->type != T_OPAREN) {
    ret = syntaxError(error, list.oParen->position, "List must begin with ')'");
    goto failure;
  }

  while (true) {

    Token *cParen;
    ret = tryStreamPeek(stream, &cParen, error);
    if (ret != R_SUCCESS) {
      if (ret == R_EOF) { // eof too soon
        ret = syntaxError(error, list.oParen->position, "Encountered EOF, was expecting ')'");
      }
      goto failure;
    }

    if (cParen->type == T_CPAREN) { // found our closed paren

      Expr *expr = malloc(sizeof(Expr));
      if (expr == NULL) {
        ret = memoryError(error, "malloc Expr");
        goto failure;
      }

      streamDropPeeked(stream); // consume cParen now that nothing else can fail
      list.cParen = cParen;

      expr->type = N_LIST;
      expr->list = list;

      *ptr = expr;
      return R_SUCCESS;
    }
    else { // read a new expression and add it to the list

      ret = tryExprMake(stream, &subexpr, error);
      if (ret != R_SUCCESS) {
        goto failure;
      }

      ret = tryListAppend(&list, subexpr, error);
      if (ret != R_SUCCESS) {
        goto failure;
      }
      subexpr = NULL; // subexpr is part of list now
    }
  }

  failure:
  listFreeContents(&list);
  if (subexpr != NULL) {
    exprFree(subexpr);
  }
  return ret;
}

RetVal tryListAppend(ExprList *list, Expr *expr, Error *error) {

  ListElement *elem =  malloc(sizeof(ListElement));
  if (elem == NULL) {
    return memoryError(error, "malloc ExprList");
  }

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

  elem->expr = expr;
  list->length = list->length + 1;

  return R_SUCCESS;
}

void listFreeContents(ExprList *list) {

  if (list->oParen != NULL) {
    tokenFree(list->oParen);
  }
  if (list->cParen != NULL) {
    tokenFree(list->cParen);
  }

  while (list->head != NULL) {
    ListElement *elem = list->head;
    list->head = list->head->next;
    exprFree(elem->expr);
    free(elem);
  }
}

RetVal tryQuoteMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;

  Expr *quote;
  Expr *subexpr;
  Expr *expr;

  ret = trySpecialMake(N_QUOTE, L"quote", stream, &quote, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryExprMake(stream, subexpr, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  expr = malloc(sizeof(Expr));
  if (expr == NULL) {
    ret = memoryError(error, "malloc Expr");
    goto failure;
  }

  expr->type = N_LIST;
  expr->list.oParen = NULL; // none exists
  expr->list.cParen = NULL; // none exists

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

RetVal tryExprMake(TokenStream *stream, Expr **ptr, Error *error) {

  RetVal ret;
  Token *peek;

  ret = tryStreamPeek(stream, &peek, error);
  if (ret != R_SUCCESS) {
    return ret;
  }
  // TODO: probably need to handle EOF here as well, you could call #tryExprMake on an empty stream and it should return EOF, not ERROR

  switch (peek->type) {
    case T_NUMBER:
      ret = tryNumberMake(stream, ptr, error);
      break;
    case T_STRING:
      ret = tryStringMake(stream, ptr, error);
      break;
    case T_SYMBOL:
      ret = trySymbolMake(stream, ptr, error);
      break;
    case T_KEYWORD:
      ret = tryKeywordMake(stream, ptr, error);
      break;
    case T_TRUE:
    case T_FALSE:
      ret = tryBooleanMake(stream, ptr, error);
      break;
    case T_NIL:
      ret = trySpecialMake(N_NIL, L"nil", stream, ptr, error);
      break;
    case T_OPAREN:
      ret = tryListMake(stream, ptr, error);
      break;
    case T_QUOTE:
        ret = tryQuoteMake(stream, ptr, error);
      break;
    default:
      ret = syntaxError(error, peek->position, "Unknown token type");
  }

  if (ret != R_SUCCESS) {
    return ret;
  }
  return R_SUCCESS;
}

void exprFree(Expr *expr) {
  if (expr->type == N_ATOM) {
    free(expr->atom);
  }
  else if (expr->type == N_LIST) {
    listFreeContents(&(expr->list));
  }
  free(expr);
}

