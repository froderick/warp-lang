#ifndef WARP_LANG_COMPILER_H
#define WARP_LANG_COMPILER_H

#include "../bootstrap/pool.h"
#include "../bootstrap/ast.h"
#include "../bytecode.h"

void compileTopLevel(Pool_t pool, FormRoot *root, CodeUnit *codeUnit);

#endif //WARP_LANG_COMPILER_H
