#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <wctype.h>
#include <errno.h>

#include "utils.h"
#include "lexer.h"

/*
 * Token Model and Lexer
 */

typedef struct LexerState {
  unsigned long position;
  unsigned long lineNumber;
  unsigned long colNumber;
  StringBuffer_t b;
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

RetVal tryTokenInit(TokenType type, wchar_t *text, uint64_t position, uint64_t length,
                    uint64_t lineNumber, uint64_t colNumber, Token **ptr, Error *error) {

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
  uint64_t length = stringBufferLength(s->b);
  wchar_t *text = stringBufferText(s->b);
  return tryTokenInit(type, text, s->position, length, s->lineNumber, s->colNumber, ptr, error);
}

void tokenFree(Token *t) {
  free(t);
}

RetVal tryLexerStateMake(LexerState **ptr, Error *error) {

  StringBuffer_t b = NULL;
  LexerState *s = NULL;

  if (tryStringBufferMake(&b, error) != R_SUCCESS) {
    return R_ERROR;
  }

  s = malloc(sizeof(LexerState));
  if (s == NULL) {
    stringBufferFree(b);
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
    stringBufferFree(s->b);
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

RetVal tryReadString(InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryStringBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is a non-escaped quote

  bool foundEnd = false;
  bool escape = false;
  wint_t ch;
  do {

    int read = tryInputStreamReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      return tokenizationError(error, s->position, "unexpected EOF, string must end in a '\"''");
    }

    if (tryStringBufferAppend(s->b, ch, error) != R_SUCCESS) {
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

RetVal tryReadNumber(InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryStringBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not numeric, then push back

  bool matched;
  wint_t ch;
  do {

    int read = tryInputStreamReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      matched = false;
    }
    else if (!iswdigit(ch)) {
      if (tryInputStreamUnreadChar(source, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = false;
    }
    else {
      if (tryStringBufferAppend(s->b, ch, error) != R_SUCCESS) {
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

bool isSymbolStart(wchar_t ch) {
  return iswalpha(ch)
         || ch == L'+'
         || ch == L'-'
         || ch == L'!'
         || ch == L'*';
}

bool isSymbolContinue(wchar_t ch) {
  return iswalnum(ch)
         || ch == L'+'
         || ch == L'-'
         || ch == L'!'
         || ch == L'*';
}

RetVal tryReadSymbol(InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryStringBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back
  // on EOF, stop reading and return R_SUCCESS

  bool matched;
  wchar_t ch;
  do {

    int read = tryInputStreamReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      matched = false;
    }
    else if (!isSymbolContinue(ch)) {
      if (tryInputStreamUnreadChar(source, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = false;
    }
    else {
      if (tryStringBufferAppend(s->b, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = true;
    }

  } while (matched);

  wchar_t *text = stringBufferText(s->b);

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

RetVal tryReadKeyword(InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {

  if (tryStringBufferAppend(s->b, first, error) != R_SUCCESS) {
    return R_ERROR;
  }

  // keep reading until char is not alphanumeric, then push back

  bool matched;
  wint_t ch;
  do {

    int read = tryInputStreamReadChar(source, &ch, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }

    if (read == R_EOF) {
      matched = false;
    }
    else if (!iswalnum(ch)) {
      if (tryInputStreamUnreadChar(source, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = false;
    }
    else {
      if (tryStringBufferAppend(s->b, ch, error) != R_SUCCESS) {
        return R_ERROR;
      }
      matched = true;
    }

  } while (matched);

  if (stringBufferLength(s->b) == 1) {
    return tokenizationError(error, s->position, "keyword token type cannot be empty");
  }

  if (tryTokenInitFromLexer(s, T_KEYWORD, token, error) != R_SUCCESS) {
    return R_ERROR;
  }

  return R_SUCCESS;
}

RetVal tryTokenRead(InputStream_t source, LexerState *s, Token **token, Error *error) {

  stringBufferClear(s->b);

  wint_t ch;

  while (true) {
    int read = tryInputStreamReadChar(source, &ch, error);
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
  else if (isSymbolStart(ch)) {
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
    throwTokenizationError(error, s->position, "unexpected character '%lc'\n", ch);
  }

  // if we created a token, increment the lexer position
  if (ret != R_ERROR && *token != NULL) {
    s->position = s->position + (*token)->source.length;
    s->colNumber = s->colNumber + (*token)->source.length;
  }

  return ret;

  failure:
    return ret;
}

/*
 * A stream-based public API for the lexer. Makes it easy to iterate over
 * tokens. Lets you peek one token ahead. Allocates memory for each token,
 * which must be freed after use by calling #tokenFree().
 */

typedef struct TokenStream {
  InputStream_t source;
  LexerState *lexer;
  Token* next;
} TokenStream;

RetVal tryStreamMake(InputStream_t source, TokenStream **ptr, Error *error) {

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

  int closeError = tryInputStreamFree(s->source, error);

  lexerStateFree(s->lexer);
  tokenFree(s->next);
  free(s);

  return closeError;
}
