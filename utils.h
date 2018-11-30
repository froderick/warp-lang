#ifndef WARP_LANG_UTILS_H
#define WARP_LANG_UTILS_H

#include <wctype.h>
#include "errors.h"

RetVal tryCopyText(wchar_t* from, wchar_t **ptr, uint64_t len, Error *error);

#endif //WARP_LANG_UTILS_H
