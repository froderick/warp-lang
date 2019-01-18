#include "utils.h"

#include <stdlib.h>
#include <wchar.h>
#include <strings.h>

/*
 * Allocate a new string of the same length, and copy the original string into
 * it.
 */
RetVal tryCopyText(wchar_t* from, wchar_t **ptr, uint64_t len, Error *error) {

  wchar_t *to = malloc((sizeof(wchar_t) * len) + 1);
  if (to == NULL) {
    return memoryError(error, "malloc wchar_t string");
  }
  wcsncpy(to, from, len);
  to[len] = L'\0';

  *ptr = to;
  return R_SUCCESS;
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

  InputStream *s;

  if (NULL == (s = malloc(sizeof(InputStream)))) {
    return memoryError(error, "malloc InputStream");
  }

  s->state = state;
  s->readChar = readChar;
  s->unreadChar = unreadChar;
  s->freeState = freeState;

  *ptr = s;
  return R_SUCCESS;
}

RetVal tryInputStreamFree(InputStream *s, Error *error) {

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

  FILE *file;

  file = fopen(filename, "r");
  if (file == NULL) {
    return ioError(error, "making stream from file");
  }

  if (tryFileInputStreamMake(file, s, error) != R_SUCCESS) {
    if (!fclose(file)) {
      return ioError(error, "closing file stream");
    }
    return R_ERROR;
  }

  return R_SUCCESS;
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

  StringInputStream *state;

  if (NULL == (state = malloc(sizeof(StringInputStream)))) {
    return memoryError(error, "malloc StringInputStream");
  }

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

  StringBuffer *b;
  wchar_t *data;

  if (NULL == (b = malloc(sizeof(StringBuffer)))) {
    return memoryError(error, "malloc StringBuffer");
  }

  b->usedChars = 0;
  b->allocatedChars = 256;

  if (NULL == (data = malloc(stringBufferAllocatedBytes(b)))) {
    free(b);
    return memoryError(error, "malloc StringBuffer array");
  }

  bzero(data, stringBufferAllocatedBytes(b));

  b->data = data;
  *ptr = b;
  return R_SUCCESS;
}

void stringBufferFree(StringBuffer *b) {
  if (b != NULL) {
    free(b->data);
  }
  free(b);
}

RetVal tryStringBufferAppendChar(StringBuffer *b, wchar_t ch, Error *error) {

  if (b->usedChars + 1 == (b->allocatedChars - 1)) {

    unsigned long oldSizeInBytes = stringBufferAllocatedBytes(b);
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

RetVal tryStringBufferAppendStr(StringBuffer *b, wchar_t *str, Error *error) {

  uint64_t len = wcslen(str);

  if (b->usedChars + len > (b->allocatedChars - 1)) {

    unsigned long oldSizeInBytes = stringBufferAllocatedBytes(b);
    unsigned long newSizeInBytes = (oldSizeInBytes + (sizeof(wchar_t) * len)) * 2;

    b->data = realloc(b->data, newSizeInBytes);
    if (b->data == NULL) {
      return memoryError(error, "realloc StringBuffer array");
    }

    b->allocatedChars = b->allocatedChars * 2;
  }

  memcpy(b->data + b->usedChars, str, len * sizeof(wchar_t));
  b->usedChars = b->usedChars + len;
  b->data[b->usedChars] = L'\0';

  return R_SUCCESS;
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

