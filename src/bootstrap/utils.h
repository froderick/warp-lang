#ifndef WARP_LANG_UTILS_H
#define WARP_LANG_UTILS_H

#include <wctype.h>
#include "../errors.h"
#include "pool.h"

/*
 * Basic string utilities
 */

wchar_t* copyText(Pool_t pool, wchar_t* from, uint64_t len);

/*
 * Input/Output Stream Abstraction
 *
 * Mainly did this because not all operating systems have the builtin concept
 * of a 'string stream' (looking at you, MacOS).
 */

typedef struct InputStream *InputStream_t;

RetVal tryInputStreamMake(
    Pool_t pool,
    void *state,
    RetVal (*readChar)(void *state, wchar_t *ch, Error *error),
    RetVal (*unreadChar)(void *state, wchar_t ch, Error *error),
    RetVal (*freeState)(void *state, Error *error),
    InputStream_t *ptr,
    Error *error
);
RetVal tryInputStreamFree(InputStream_t input, Error *error); // TODO: close and free should probably not be the same thing

// supported operations
RetVal tryInputStreamReadChar(InputStream_t input, wchar_t *ch, Error *error);
RetVal tryInputStreamUnreadChar(InputStream_t input, wchar_t ch, Error *error);

// source factories
RetVal tryFileInputStreamMake(Pool_t pool, FILE *file, InputStream_t *s, Error *error);
RetVal tryFileInputStreamMakeFilename(Pool_t pool, char *filename, InputStream_t *s, Error *error);
RetVal tryStringInputStreamMake(Pool_t pool, wchar_t *text, uint64_t length, InputStream_t *s, Error *error);

/*
 * Auto-expanding string buffer implementation.
 * Built on wide chars, so UTF8 friendly.
 */

typedef struct StringBuffer *StringBuffer_t;

StringBuffer_t stringBufferMake(Pool_t pool);

void stringBufferAppendChar(StringBuffer_t b, wchar_t ch);
void stringBufferAppendStr(StringBuffer_t b, wchar_t *str);

uint64_t stringBufferLength(StringBuffer_t b);
wchar_t* stringBufferText(StringBuffer_t b);
void stringBufferClear(StringBuffer_t b);

uint64_t stringBufferAllocatedBytes(StringBuffer_t buf);
uint64_t stringBufferUnusedBytes(StringBuffer_t buf);

typedef struct Text {
  wchar_t *value;
  uint64_t length;
} Text;

void textInitContents(Text *text);
void textMake(Pool_t pool, wchar_t* from, Text *text, uint64_t len);
void textCopy(Pool_t pool, Text *from, Text *to);

#endif //WARP_LANG_UTILS_H


