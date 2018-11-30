#include "utils.h"

#include <stdlib.h>
#include <wchar.h>

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
