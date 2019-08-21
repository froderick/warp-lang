#ifndef WARP_LANG_EXPANDER_H
#define WARP_LANG_EXPANDER_H

#include "ast.h"
#include "../vm/vm.h"
#include "pool.h"

/*
 * Things that have to be done to implement macros:
 * X create builtin support to 'mark' vars as containing valid macro functions
 * - implement expander, which can use a VM instance to locate macros, invoke them, and return the results
 * - pass the expander to the analyzer as a parameter, make the analyzer use it when it can't find a binding for a symbol
 *   - if it returns a NULL result, the symbol must refer to a var
 *   - if it returns a non-NULL result, the symbol refers to a macro
 *     - the resulting expression should replace the original one in the expr tree
 *     - the old expression should be freed
 *     - analysis should continue to recurse into the new expression
 *
 * - how can macroexpand be implemented?
 *   - I guess this is part of implementing (eval), which has a superset of the same requirements
 *   - the VM has to support a builtin that invokes the reader, analyzer, compiler, vm evaluation, etc.
 */

typedef struct Expander *Expander_t;

RetVal tryMakeExpander(Pool_t pool, Expander_t *expander, VM_t vm, Error *error);
void freeExpander(Expander_t expander);

RetVal tryIsMacro(Expander_t expander, Text sym, bool *isMacro, Error *error);

RetVal tryExpand(Expander_t expander, Text sym, Form *input, Form **output, Error *error);

#endif //WARP_LANG_EXPANDER_H
