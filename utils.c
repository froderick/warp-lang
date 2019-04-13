#include "utils.h"

#include <stdlib.h>
#include <wchar.h>
#include <string.h>
#include <strings.h>

/*
 * Allocate a new string of the same length, and copy the original string into
 * it.
 */
RetVal tryCopyText(wchar_t* from, wchar_t **ptr, uint64_t len, Error *error) {
  RetVal ret;

  wchar_t *to;
  tryMalloc(to, sizeof(wchar_t) * (len + 1), "wchar_t string");

  wcsncpy(to, from, len);
  to[len] = L'\0';

  *ptr = to;
  return R_SUCCESS;

  failure:
    return ret;
}

/*
 * Input/Output Stream Abstraction
 *
 * Mainly did this because not all operating systems have the builtin concept
 * of a 'string stream' (looking at you, MacOS).
 */

typedef struct InputStream {
  void *state;
  RetVal (*readChar)(void *state, wchar_t *ch, Error *error);
  RetVal (*unreadChar)(void * state, wchar_t ch, Error *error);
  RetVal (*freeState)(void *state, Error *error);
} InputStream;

RetVal tryInputStreamMake(
    void *state,
    RetVal (*readChar)(void *state, wchar_t *ch, Error *error),
    RetVal (*unreadChar)(void *state, wchar_t ch, Error *error),
    RetVal (*freeState)(void *state, Error *error),
    InputStream **ptr,
    Error *error) {
  RetVal ret;

  InputStream *s;

  tryMalloc(s, sizeof(InputStream), "InputStream");

  s->state = state;
  s->readChar = readChar;
  s->unreadChar = unreadChar;
  s->freeState = freeState;

  *ptr = s;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryInputStreamFree(InputStream *s, Error *error) {
  RetVal ret;

  if (s->freeState != NULL) {
    ret = s->freeState(s->state, error);
  }
  free(s);

  return ret;
}

RetVal tryReadCharFromFILE(void *state, wchar_t* ch, Error *error) {
  RetVal ret;

  FILE *stream = (FILE*)state;

  *ch = fgetwc(stream);
  if (*ch == WEOF) {
    if (feof(stream)) {
      return R_EOF;
    }
    else {
      throwIOError(error, "read token from stream");
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryUnreadCharToFILE(void *state, wchar_t ch, Error *error) {
  RetVal ret;

  FILE *stream = (FILE*)state;

  wint_t result = ungetwc(ch, stream);
  if (result == WEOF) {
    throwIOError(error, "push character back onto stream");
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryFreeFILE(void *state, Error *error) {
  RetVal ret;

  FILE *stream = (FILE*)state;

  if (stream != NULL && fclose(stream)) {
    throwIOError(error, "closing stream on free");
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryFileInputStreamMake(FILE *file, InputStream **s, Error *error) {
  return tryInputStreamMake(
      file,
      tryReadCharFromFILE,
      tryUnreadCharToFILE,
      tryFreeFILE,
      s,
      error);
}

RetVal tryFileInputStreamMakeFilename(char *filename, InputStream **s, Error *error) {
  RetVal ret;

  FILE *file = NULL;

  file = fopen(filename, "r");
  if (file == NULL) {
    throwIOError(error, "making stream from file");
  }

  throws(tryFileInputStreamMake(file, s, error));

  return R_SUCCESS;

  failure:
    if (file != NULL) {
      if (!fclose(file)) {
        return ioError(error, "closing file stream");
      }
    }
    return ret;
}

typedef struct StringInputStream {
  wchar_t* text;
  uint64_t length;
  uint64_t next;
} StringInputStream;

RetVal tryReadCharFromString(void *state, wchar_t* ch, Error *error) {

  StringInputStream *stream = (StringInputStream*)state;

  if (stream->next == stream->length) {
    return R_EOF;
  }

  *ch = stream->text[stream->next];
  stream->next = stream->next + 1;
  return R_SUCCESS;
}

RetVal tryUnreadCharToString(void *state, wchar_t ch, Error *error) {

  StringInputStream *stream = (StringInputStream*)state;

  if (stream->next > 0) {
    stream->next = stream->next - 1;
  }

  return R_SUCCESS;
}

RetVal tryStringInputStreamFree(void *state, Error *error) {
  StringInputStream *stream = (StringInputStream*)state;
  free(stream);
  return R_SUCCESS;
}

RetVal tryStringInputStreamMake(wchar_t *text, uint64_t length, InputStream **s, Error *error) {
  RetVal ret;

  StringInputStream *state;

  tryMalloc(state, sizeof(StringInputStream), "StringInputStream");

  state->text = text;
  state->length = length;
  state->next = 0;

  return tryInputStreamMake(
      (void *) state,
      tryReadCharFromString,
      tryUnreadCharToString,
      tryStringInputStreamFree,
      s,
      error);

  failure:
    return ret;
}

RetVal tryInputStreamReadChar(InputStream *source, wchar_t *ch, Error *error) {
  return source->readChar(source->state, ch, error);
}

RetVal tryInputStreamUnreadChar(InputStream *source, wchar_t ch, Error *error) {
  return source->unreadChar(source->state, ch, error);
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

void stringBufferInitContents(StringBuffer *b) {
  b->allocatedChars = 0;
  b->usedChars = 0;
  b->data = NULL;
}

void stringBufferFreeContents(StringBuffer *b) {
  if (b != NULL) {
    b->allocatedChars = 0;
    b->usedChars = 0;
    if (b->data != NULL) {
      free(b->data);
      b->data = NULL;
    }
  }
}

uint64_t stringBufferAllocatedBytes(StringBuffer *buf) {
  return sizeof(wchar_t) * buf->allocatedChars;
}

uint64_t stringBufferUnusedBytes(StringBuffer *buf) {
  return sizeof(wchar_t) * buf->usedChars;
}

RetVal tryStringBufferMake(StringBuffer **ptr, Error *error) {
  RetVal ret;

  StringBuffer *b = NULL;
  wchar_t *data = NULL;

  tryMalloc(b, sizeof(StringBuffer), "StringBuffer");

  b->usedChars = 0;
  b->allocatedChars = 256;

  tryMalloc(data, stringBufferAllocatedBytes(b), "StringBuffer array");
  bzero(data, stringBufferAllocatedBytes(b));

  b->data = data;
  *ptr = b;
  return R_SUCCESS;

  failure:
    if (b != NULL) {
      free(b);
    }
    return ret;
}

void stringBufferFree(StringBuffer *b) {
  if (b != NULL) {
    free(b->data);
  }
  free(b);
}

RetVal tryStringBufferAppendChar(StringBuffer *b, wchar_t ch, Error *error) {
  RetVal ret;

  if (b->usedChars + 1 == (b->allocatedChars - 1)) {

    unsigned long oldSizeInBytes = stringBufferAllocatedBytes(b);
    unsigned long newSizeInBytes = oldSizeInBytes * 2;

    b->data = realloc(b->data, newSizeInBytes);
    if (b->data == NULL) {
      throwMemoryError(error, "realloc StringBuffer array");
    }

    b->allocatedChars = b->allocatedChars * 2;
  }

  b->data[b->usedChars] = ch;
  b->usedChars = b->usedChars + 1;
  b->data[b->usedChars] = L'\0';

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryStringBufferAppendStr(StringBuffer *b, wchar_t *str, Error *error) {
  RetVal ret;

  uint64_t len = wcslen(str);

  if (b->usedChars + len > (b->allocatedChars - 1)) {

    unsigned long oldSizeInBytes = stringBufferAllocatedBytes(b);
    unsigned long newSizeInBytes = (oldSizeInBytes + (sizeof(wchar_t) * len)) * 2;

    b->data = realloc(b->data, newSizeInBytes);
    if (b->data == NULL) {
      throwMemoryError(error, "realloc StringBuffer array");
    }

    b->allocatedChars = b->allocatedChars * 2;
  }

  memcpy(b->data + b->usedChars, str, len * sizeof(wchar_t));
  b->usedChars = b->usedChars + len;
  b->data[b->usedChars] = L'\0';

  return R_SUCCESS;

  failure:
    return ret;
}

uint64_t stringBufferLength(StringBuffer_t b) {
  return b->usedChars;
}

wchar_t* stringBufferText(StringBuffer_t b) {
  return b->data;
}

void stringBufferClear(StringBuffer *b) {
  bzero(b->data, stringBufferUnusedBytes(b));
  b->usedChars = 0;
}

void textInitContents(Text *text) {
  text->length = 0;
  text->value = NULL;
}

RetVal tryTextMake(wchar_t* from, Text *text, uint64_t len, Error *error) {
  RetVal ret;

  text->length = len;
  throws(tryCopyText(from, &text->value, text->length, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryTextCopy(Text *from, Text *to, Error *error) {
  RetVal ret;

  to->length = from->length;
  throws(tryCopyText(from->value, &to->value, to->length, error));

  return R_SUCCESS;

  failure:
  return ret;
}

void textFreeContents(Text *text) {
  text->length = 0;
  if (text->value != NULL) {
    free(text->value);
    text->value = NULL;
  }
}
