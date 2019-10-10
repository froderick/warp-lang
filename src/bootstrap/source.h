#ifndef WARP_LANG_SOURCE_H
#define WARP_LANG_SOURCE_H

#include <stdint.h>

/*
 * Widely-used representations of source-level debug information.
 */

typedef struct SourceLocation {
  bool isSet;
  uint64_t position;
  uint64_t lineNumber;
  uint64_t colNumber;
  uint64_t length;
} SourceLocation;

void sourceLocationInitContents(SourceLocation *l);
void sourceLocationFreeContents(SourceLocation *l);

#endif //WARP_LANG_SOURCE_H
