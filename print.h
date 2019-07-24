#ifndef WARP_LANG_PRINT_H
#define WARP_LANG_PRINT_H

#include "reader.h"
#include "vm.h"

Expr* printToReader(VM_t vm, Pool_t pool, Value result);

void print(VM_t vm, Value result);

void printException(VM_t vm, Value exception);

#endif //WARP_LANG_PRINT_H


