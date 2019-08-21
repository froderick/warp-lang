#ifndef WARP_LANG_COMPILER_H
#define WARP_LANG_COMPILER_H

#include "../vm/vm.h"
#include "analyzer.h"

void compileTopLevel(Pool_t pool, FormRoot *root, CodeUnit *codeUnit);

#endif //WARP_LANG_COMPILER_H
