#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>
#include "../bootstrap/print.h"
#include "../errors.h"
#include <errno.h>

#include "internal.h"

void vmConfigInitContents(VMConfig *config) {
  config->gcOnAlloc = false;
}

void vmInitContents(VM *vm, VMConfig config) {
  vm->config = config;
  vm->instTable = instTableCreate();
  vm->valueTypeTable = valueTypeTableCreate();
  GCCreate(&vm->gc, 1024 * 1000);
  stackInitContents(&vm->stack, 1024 * 1000);
  tableInit(&vm->symbolTable);
  tableInit(&vm->keywordTable);
  vm->current = NULL;
  vm->exception = W_NIL_VALUE;
  vm->noFrameRoots = NULL;
  vm->noFrameHandlers = NULL;
  initCFns(vm);
}

Value getKeyword(VM *vm, wchar_t *text) {
  Value protectedName = makeStringValue(vm, text, wcslen(text));
  pushFrameRoot(vm, &protectedName);
  Value kw = keywordIntern(vm, &protectedName);
  popFrameRoot(vm); // protectedMessageName
  return kw;
}

Value getSymbol(VM *vm, wchar_t *text) {
  Value protectedName = makeStringValue(vm, text, wcslen(text));
  pushFrameRoot(vm, &protectedName);
  Value kw = symbolIntern(vm, &protectedName);
  popFrameRoot(vm); // protectedMessageName
  return kw;
}

void vmFreeContents(VM *vm) {
  if (vm != NULL) {
    GCFreeContents(&vm->gc);
    // TODO: seems like we're missing a few things here
    tableFreeContents(&vm->symbolTable);
  }
}

VM_t vmMake(VMConfig config) {
  VM *vm = malloc(sizeof(VM));
  if (vm == NULL) {
    explode("failed to malloc");
  }
  vmInitContents(vm, config);
  return vm;
}

void vmFree(VM *vm) {
  if (vm != NULL) {
    vmFreeContents(vm);
    free(vm);
  }
}

/*
 * code printing utils based on InstTable metadata
 */

static void _printCodeArray(InstTable *table, uint8_t *code, uint16_t codeLength) {
  for (int i=0; i<codeLength; i++) {
    Inst inst = table->instructions[code[i]];
    inst.print(&i, inst.name, code);
  }
}

static void _printFnConstant(InstTable *table, FnConstant fnConst) {

  for (uint16_t i=0; i<fnConst.numConstants; i++) {
    Constant c = fnConst.constants[i];
    if (c.type == CT_FN) {
      printf("constant fn within constant fn %u:\n", i);
      _printFnConstant(table, c.function);
    }
  }

  printf("fn const code:\n");
  _printCodeArray(table, fnConst.code.code, fnConst.code.codeLength);
}

static void _printCodeUnit(InstTable *table, CodeUnit *unit) {

  for (uint16_t i=0; i<unit->numConstants; i++) {
    Constant c = unit->constants[i];
    if (c.type == CT_FN) {
      printf("constant fn %u:\n", i);
      _printFnConstant(table, c.function);
    }
  }

  printf("code:\n");
  _printCodeArray(table, unit->code.code, unit->code.codeLength);
}

void printCodeArray(uint8_t *code, uint16_t codeLength) {
  InstTable table = instTableCreate();
  _printCodeArray(&table, code, codeLength);
}

void printCodeUnit(CodeUnit *unit) {
  InstTable table = instTableCreate();
  _printCodeUnit(&table, unit);
}
