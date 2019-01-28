#ifndef WARP_LANG_COMPILER_H
#define WARP_LANG_COMPILER_H

#include "vm.h"
#include "analyzer.h"

RetVal tryCompileTopLevel(FormRoot *form, CodeUnit *codeUnit, Error *error);

#endif //WARP_LANG_COMPILER_H
