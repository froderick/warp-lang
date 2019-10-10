#include <stdbool.h>
#include <stddef.h>
#include "source.h"

void sourceLocationInitContents(SourceLocation *l) {
  l->isSet = false;
  l->position = 0;
  l->lineNumber = 0;
  l->colNumber = 0;
  l->length = 0;
}

void sourceLocationFreeContents(SourceLocation *l) {
  if (l != NULL) {
    sourceLocationInitContents(l);
  }
}
