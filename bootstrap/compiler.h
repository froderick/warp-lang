#ifndef WARP_LANG_COMPILER_H
#define WARP_LANG_COMPILER_H

#include "../vm/vm.h"
#include "analyzer.h"

RetVal tryCompileTopLevel(Pool_t pool, FormRoot *form, CodeUnit *codeUnit, Error *error);

#endif //WARP_LANG_COMPILER_H
