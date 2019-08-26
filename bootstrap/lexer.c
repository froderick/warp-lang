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
#include "pool.h"

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
    case T_SYNTAX_QUOTE:
      return "SYNTAX_QUOTE";
    case T_UNQUOTE:
      return "UNQUOTE";
    case T_SPLICING_UNQUOTE:
      return "SPLICING_UNQUOTE";
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
    case T_CHAR:
      return "CHAR";
    case T_STRING:
      return "STRING";
    case T_KEYWORD:
      return "KEYWORD";
    case T_COMMENT:
      return "COMMENT";
    case T_OBRACKET:
      return "OBRACKET";
    case T_CBRACKET:
      return "CBRACKET";
    default:
      return "<UNKNOWN>";
  }
}

Token* tokenInit(Pool_t pool, TokenType type, wchar_t *text, uint64_t position, uint64_t length,
                    uint64_t lineNumber, uint64_t colNumber) {
  Token *t;
  palloc(pool, t, sizeof(Token) + (sizeof(wchar_t) * (length + 1)), "Token");

  t->type = type;
  t->typeName = tokenName(type);
  t->source.isSet = true;
  t->source.position = position;
  t->source.length = length;
  t->source.lineNumber = lineNumber;
  t->source.colNumber = colNumber;
  wcpncpy(t->text, text, length);
  t->text[length] = L'\0';

  return t;
}

Token* tokenInitFromLexer(Pool_t pool, LexerState *s, TokenType type) {
  uint64_t length = stringBufferLength(s->b);
  wchar_t *text = stringBufferText(s->b);
  return tokenInit(pool, type, text, s->position, length, s->lineNumber, s->colNumber);
}

LexerState* lexerStateMake(Pool_t pool) {
  StringBuffer_t b = stringBufferMake(pool);

  LexerState *s = NULL;
  palloc(pool, s, sizeof(LexerState), "LexerState");
  s->position = 0;
  s->lineNumber = 1;
  s->colNumber = 1;
  s->b = b;

  return s;
}

bool isWhitespace(wchar_t ch) {
  return ch == L'\n'
         || ch == L' '
         || ch == L'\t';
}

bool isNewline(wchar_t ch) {
  return ch == L'\n';
}

RetVal tryReadString(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  stringBufferAppendChar(s->b, first);

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
      throwTokenizationError(error, s->position, "unexpected EOF, string must end in a '\"''");
    }

    stringBufferAppendChar(s->b, ch);

    if (!escape && ch == L'"') {
      foundEnd = true;
    }
    else {
      escape = !escape && ch == L'\\';
    }
  }
  while (!foundEnd);

  *token = tokenInitFromLexer(pool, s, T_STRING);

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryReadUnquote(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  stringBufferAppendChar(s->b, first);

  // read a second char, pick one of the two unquotes, push back if it is not the two-character one

  wint_t ch;
  throws(tryInputStreamReadChar(source, &ch, error));

  if (ch != L'@') {
    throws(tryInputStreamUnreadChar(source, ch, error));
    *token = tokenInitFromLexer(pool, s, T_UNQUOTE);
  }
  else {
    stringBufferAppendChar(s->b, ch);
    *token = tokenInitFromLexer(pool, s, T_SPLICING_UNQUOTE);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryReadNumber(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  stringBufferAppendChar(s->b, first);

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
      throws(tryInputStreamUnreadChar(source, ch, error));
      matched = false;
    }
    else {
      stringBufferAppendChar(s->b, ch);
      matched = true;
    }

  } while (matched);

  *token = tokenInitFromLexer(pool, s, T_NUMBER);

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryReadCharOrQuote(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  wchar_t second;
  int read = tryInputStreamReadChar(source, &second, error);
  if (read == R_ERROR) {
    return R_ERROR;
  }
  else if (read == R_EOF) { // quote
    stringBufferAppendChar(s->b, first);
    *token = tokenInitFromLexer(pool, s, T_QUOTE);
  }
  else {
    wchar_t third;
    int read = tryInputStreamReadChar(source, &third, error);
    if (read == R_ERROR) {
      return R_ERROR;
    }
    else if (read == R_EOF) { // quote
      throws(tryInputStreamUnreadChar(source, second, error));
      stringBufferAppendChar(s->b, first);
      *token = tokenInitFromLexer(pool, s, T_QUOTE);
    }
    else {
      if (third == first) {
        stringBufferAppendChar(s->b, second);
        *token = tokenInitFromLexer(pool, s, T_CHAR);
      }
      else { // quote
        throws(tryInputStreamUnreadChar(source, third, error));
        throws(tryInputStreamUnreadChar(source, second, error));
        stringBufferAppendChar(s->b, first);
        *token = tokenInitFromLexer(pool, s, T_QUOTE);
      }
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
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
         || ch == L'='
         || ch == L'_'
         || ch == L'?'
         || ch == L'<'
         || ch == L'>'
         || ch == L'*';
}

bool isSymbolContinue(wchar_t ch) {
  return iswalnum(ch)
         || ch == L'+'
         || ch == L'-'
         || ch == L'!'
         || ch == L'='
         || ch == L'_'
         || ch == L'?'
         || ch == L'/'
         || ch == L'>'
         || ch == L'<'
         || ch == L'*';
}

RetVal tryReadSymbol(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  stringBufferAppendChar(s->b, first);

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
      throws(tryInputStreamUnreadChar(source, ch, error));
      matched = false;
    }
    else {
      stringBufferAppendChar(s->b, ch);
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

  *token = tokenInitFromLexer(pool, s, type);

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryReadKeyword(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  stringBufferAppendChar(s->b, first);

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
      throws(tryInputStreamUnreadChar(source, ch, error));
      matched = false;
    }
    else {
      stringBufferAppendChar(s->b, ch);
      matched = true;
    }

  } while (matched);

  if (stringBufferLength(s->b) == 1) {
    throwTokenizationError(error, s->position, "keyword token type cannot be empty");
  }

  *token = tokenInitFromLexer(pool, s, T_KEYWORD);

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryReadComment(Pool_t pool, InputStream_t source, LexerState *s, wchar_t first, Token **token, Error *error) {
  RetVal ret;

  stringBufferAppendChar(s->b, first);

  // keep reading until char is not alphanumeric, then push back

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
    else if (ch == L'\n') {
      throws(tryInputStreamUnreadChar(source, ch, error));
      matched = false;
    }
    else {
      stringBufferAppendChar(s->b, ch);
      matched = true;
    }

  } while (matched);

  *token = tokenInitFromLexer(pool, s, T_COMMENT);

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryTokenRead(Pool_t pool, InputStream_t source, LexerState *s, Token **token, Error *error) {
  RetVal ret;

  stringBufferClear(s->b);

  wchar_t ch;

  // eat whitespace
  while (true) {
    throws(tryInputStreamReadChar(source, &ch, error));

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

  // single-character tokens, do not require buffering
  if (ch == L'(') {
    *token = tokenInit(pool, T_OPAREN, L"(", s->position, 1, s->lineNumber, s->colNumber);
  }
  else if (ch == L')') {
    *token = tokenInit(pool, T_CPAREN, L")", s->position, 1, s->lineNumber, s->colNumber);
  }
  else if (ch == L'[') {
    *token = tokenInit(pool, T_OVEC, L"[", s->position, 1, s->lineNumber, s->colNumber);
  }
  else if (ch == L']') {
    *token = tokenInit(pool, T_CVEC, L"]", s->position, 1, s->lineNumber, s->colNumber);
  }
  else if (ch == L'{') {
    *token = tokenInit(pool, T_OBRACKET, L"{", s->position, 1, s->lineNumber, s->colNumber);
  }
  else if (ch == L'}') {
    *token = tokenInit(pool, T_CBRACKET, L"}", s->position, 1, s->lineNumber, s->colNumber);
  }
  else if (ch == L'`') {
    *token = tokenInit(pool, T_SYNTAX_QUOTE, L"`", s->position, 1, s->lineNumber, s->colNumber);
  }

  else if (ch == L'&') {
    *token = tokenInit(pool, T_SYMBOL, L"&", s->position, 1, s->lineNumber, s->colNumber);
  }

  // multi-character tokens
  else if (ch == L'~') {
    throws(tryReadUnquote(pool, source, s, ch, token, error));
  }
  else if (iswdigit(ch)) {
    throws(tryReadNumber(pool, source, s, ch, token, error));
  }
  else if (ch == L'\'') {
    throws(tryReadCharOrQuote(pool, source, s, ch, token, error));
  }
  else if (isSymbolStart(ch)) {
    throws(tryReadSymbol(pool, source, s, ch, token, error));
  }
  else if (ch == L':') {
    throws(tryReadKeyword(pool, source, s, ch, token, error));
  }
  else if (ch == L'"') {
    throws(tryReadString(pool, source, s, ch, token, error));
  }

  // comments
  else if (ch == L';') {
    throws(tryReadComment(pool, source, s, ch, token, error));
  }

    // invalid token
  else {
    throwTokenizationError(error, s->position, "unexpected character '%lc'\n", ch);
  }

  // if we created a token, increment the lexer position
  if (*token != NULL) {
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

TokenStream* streamMake(Pool_t pool, InputStream_t source) {
  LexerState *l = lexerStateMake(pool);
  TokenStream *s = NULL;
  palloc(pool, s, sizeof(TokenStream), "TokenStream");
  s->source = source;
  s->lexer = l;
  s->next = NULL;
  return s;
}

TokenStream* tryStreamMakeFile(Pool_t pool, char *filename, TokenStream **ptr, Error *error) {
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    explode("making stream from file");
  }
  return streamMake(pool, (void*)file);
}

RetVal tryStreamNext(Pool_t pool, TokenStream *s, Token **token, Error *error) {

  // clear this so folks can free it after this call without risk of a double-free
  *token = NULL;

  if (s->next != NULL) { // eat the cached next token if we have one
    *token = s->next;
    s->next = NULL;
    return R_SUCCESS;
  }

  return tryTokenRead(pool, s->source, s->lexer, token, error);
}

RetVal tryStreamPeek(Pool_t pool, TokenStream *s, Token **token, Error *error) {

  if (s->next != NULL) { // return the cached next token if we have one
    *token = s->next;
    return R_SUCCESS;
  }

  int read = tryTokenRead(pool, s->source, s->lexer, token, error);

  if (read != R_ERROR) {
    s->next = *token;
  }

  return read;
}

void streamDropPeeked(TokenStream *s) {
  s->next = NULL;
}

