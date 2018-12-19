#ifndef WARP_LANG_UTILS_H
#define WARP_LANG_UTILS_H

#include <wctype.h>
#include "errors.h"

/*
 * Basic string utilities
 */

RetVal tryCopyText(wchar_t* from, wchar_t **ptr, uint64_t len, Error *error);

/*
 * Input/Output Stream Abstraction
 *
 * Mainly did this because not all operating systems have the builtin concept
 * of a 'string stream' (looking at you, MacOS).
 */

typedef struct InputStream *InputStream_t;

RetVal tryInputStreamMake(
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
RetVal tryFileInputStreamMake(FILE *file, InputStream_t *s, Error *error);
RetVal tryStringInputStreamMake(wchar_t *text, uint64_t length, InputStream_t *s, Error *error);

typedef struct OutputStream *OutputStream_t;

RetVal tryOutputStreamMake(
    void *state,
    RetVal (*writeWString)(void *state, wchar_t *ch, Error *error),
    RetVal (*flush)(void *state, Error *error),
    RetVal (*close)(void *state, Error *error),
    RetVal (*freeState)(void *state, Error *error),
    InputStream_t *ptr,
    Error *error
);
RetVal tryOutputStreamFree(InputStream_t input, Error *error);

// supported operations
RetVal tryOutputStreamWriteWString(OutputStream_t output, wchar_t* ch, Error *error);
RetVal tryOutputStreamFlush(OutputStream_t output, Error *error);
RetVal tryOutputStreamClose(OutputStream_t output, Error *error);

// source factories
RetVal tryFileOutputStreamMake(FILE *file, OutputStream_t output, Error *error);
RetVal tryStringOutputStreamMake(wchar_t *text, uint64_t length, OutputStream_t output, Error *error);

#endif //WARP_LANG_UTILS_H


