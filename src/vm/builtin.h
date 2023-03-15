#ifndef WARP_LANG_BUILTIN_H
#define WARP_LANG_BUILTIN_H

#include "vm.h"
#include "internal.h"

/*
 * these are exposed because they are also used to implement instructions
 */
int consEval(VM_t vm, Frame_t frame);
int addEval(VM_t vm, Frame_t frame);
int subEval(VM_t vm, Frame_t frame);
int cmpEval(VM_t vm, Frame_t frame);

void putMapEntry(VM_t vm, Map **protectedMap, Value key, Value insertMe);

void initCFns(VM_t vm);

#endif //WARP_LANG_BUILTIN_H
