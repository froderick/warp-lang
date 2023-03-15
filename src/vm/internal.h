#ifndef WARP_LANG_INTERNAL_H
#define WARP_LANG_INTERNAL_H

#include <inttypes.h>
#include "vm.h"
#include "heap.h"
#include "value.h"
#include "frame.h"
#include "symbol.h"

typedef struct VM {
  VMConfig config;
  GC gc;
  InstTable instTable;
  ValueTypeTable valueTypeTable;
  Stack stack;
  FrameRoot_t noFrameRoots;
  FrameHandler_t noFrameHandlers;
  Frame_t current;
  Table symbolTable;
  Table keywordTable;
  Value exception;
} VM;


#endif //WARP_LANG_INTERNAL_H
