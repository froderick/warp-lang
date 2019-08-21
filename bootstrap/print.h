#ifndef WARP_LANG_PRINT_H
#define WARP_LANG_PRINT_H

#include "ast.h"
#include "../vm/vm.h"

Form* printToReader(VM_t vm, Pool_t pool, Value result);

void print(VM_t vm, Value result);

void printException(VM_t vm, Value exception);

#endif //WARP_LANG_PRINT_H


