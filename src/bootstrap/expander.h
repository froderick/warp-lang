#ifndef WARP_LANG_EXPANDER_H
#define WARP_LANG_EXPANDER_H

#include "ast.h"
#include "../vm/vm.h"
#include "pool.h"

typedef struct Expander *Expander_t;

RetVal tryMakeExpander(Pool_t pool, Expander_t *expander, VM_t vm, Error *error);
bool isMacro(Expander_t expander, wchar_t *sym);
RetVal tryExpand(Expander_t expander, Text sym, Form *input, Form **output, Error *error);

#endif //WARP_LANG_EXPANDER_H
