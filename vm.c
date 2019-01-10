#include <stdlib.h>
#include <string.h>
#include "vm.h"
#include "utils.h"

/*
 * CodeUnit init/free functions
 */

void sourceTableInitContents(SourceTable *t) {
  t->lineNumbers = NULL;
  t->numLineNumbers = 0;
  t->fileName = NULL;
  t->fileNameLength = 0;
}

void sourceTableFreeContents(SourceTable *t) {
  t->fileNameLength = 0;
  if (t->fileName != NULL) {
    free(t->fileName);
    t->fileName = NULL;
  }
  t->numLineNumbers = 0;
  if (t->lineNumbers != NULL) {
    free(t->lineNumbers);
    t->lineNumbers = NULL;
  }
}

void codeInitContents(Code *code) {
  code->maxOperandStackSize = 0;
  code->numLocals = 0;
  code->hasSourceTable = false;
  code->code = NULL;
  code->codeLength = 0;
  sourceTableInitContents(&code->sourceTable);
}

void codeFreeContents(Code *code) {
  if (code != NULL) {

    code->numLocals = 0;
    code->maxOperandStackSize = 0;
    code->codeLength = 0;

    if (code->code != NULL) {
      free(code->code);
      code->code = NULL;
    }

    if (code->hasSourceTable) {
      code->hasSourceTable = false;
      sourceTableFreeContents(&code->sourceTable);
    }
  }
}

void _constantFreeContents(Constant *c);

void constantFnInitContents(FnConstant *fnConst) {
  fnConst->numArgs = 0;
  fnConst->numConstants = 0;
  fnConst->constants = NULL;
  codeInitContents(&fnConst->code);
}

void constantFnFreeContents(FnConstant *fnConst) {
  if (fnConst != NULL) {
    fnConst->numArgs = 0;
    if (fnConst->constants != NULL) {
      for (int i = 0; i < fnConst->numConstants; i++) {
        _constantFreeContents(&fnConst->constants[i]);
      }
      fnConst->numConstants = 0;
      free(fnConst->constants);
      fnConst->constants = NULL;
    }
    codeFreeContents(&fnConst->code);
  }
}

void _constantFreeContents(Constant *c) {
  if (c != NULL) {
    switch (c->type) {
      case CT_NONE:
      case CT_NIL:
        break;
      case CT_BOOL:
        c->boolean = false;
        break;
      case CT_INT:
        c->integer = 0;
        break;
      case CT_VAR_REF:
        c->varRef.nameLength = 0;
        if (c->varRef.name != NULL) {
          free(c->varRef.name);
          c->varRef.name = NULL;
        }
        break;
      case CT_STR:
        c->string.length = 0;
        if (c->string.value != NULL) {
          free(c->string.value);
          c->string.value = NULL;
        }
        break;
      case CT_FN:
        constantFnFreeContents(&c->function);
        break;
    }
  }
}

void codeUnitInitContents(CodeUnit *codeUnit) {
  codeUnit->constants = NULL;
  codeUnit->numConstants = 0;
  codeInitContents(&codeUnit->code);
}

void codeUnitFreeContents(CodeUnit *u) {
  if (u != NULL) {
    if (u->constants != NULL) {
      for (int i = 0; i < u->numConstants; i++) {
        _constantFreeContents(&u->constants[i]);
      }
      u->numConstants = 0;
      free(u->constants);
      u->constants = NULL;
    }
    codeFreeContents(&u->code);
  }
}

/*
 * Temporarily stashing the var management code here
 */

typedef struct Var {
  wchar_t *namespace;
  wchar_t *name;
  Value value;
} Var;

typedef struct VarList {
  uint64_t allocatedLength;
  uint64_t length;
  Var *vars;
} VarList;

typedef struct Namespace {
  wchar_t *name;
  VarList localVars;
  Var *importedVars;
  uint64_t numImportedVars;
} Namespace;

typedef struct Namespaces {
  Namespace *namespaces;
  uint64_t numNamespaces;
  Namespace *currentNamespace;
} Namespaces;

void varFree(Var *var);

RetVal tryVarInit(wchar_t *namespace, wchar_t *name, Var *var, Error *error) {
  RetVal ret;

  var->namespace = NULL;
  var->name = NULL;
  var->value.type = VT_NIL;
  var->value.value = 0;

  throws(tryCopyText(namespace, &var->namespace, wcslen(namespace), error));
  throws(tryCopyText(name, &var->name, wcslen(name), error));

  return R_SUCCESS;

  failure:
  varFree(var);
  return ret;
}

void varFreeContents(Var *var) {
  if (var != NULL) {
    if (var->namespace != NULL) {
      free(var->namespace);
      var->namespace = NULL;
    }
    if (var->name != NULL) {
      free(var->name);
      var->name = NULL;
    }
  }
}

void varFree(Var *var) {
  if (var != NULL) {
    varFreeContents(var);
    free(var);
  }
}

Var* resolveVar(Namespaces *analyzer, wchar_t *symbolName, uint64_t symbolNameLength) {

  Namespace *ns;
  wchar_t *searchName;
  uint64_t searchNameLength;

  // assume unqualified namespace at first
  ns = analyzer->currentNamespace;
  searchName = symbolName;
  searchNameLength = symbolNameLength;

  if (symbolNameLength > 2) { // need at least three characters to qualify a var name with a namespace: ('q/n')

    // attempt to find a qualified namespace
    wchar_t *slashPtr = wcschr(symbolName, L'/');
    if (slashPtr != NULL) { // qualified
      uint64_t nsLen = slashPtr - symbolName;

      for (int i=0; i<analyzer->numNamespaces; i++) {

        Namespace *thisNs = &analyzer->namespaces[i];
        if (wcsncmp(symbolName, thisNs->name, nsLen) == 0) {
          ns = thisNs;
          searchName = slashPtr + 1;
          searchNameLength = symbolNameLength - (nsLen + 1);
          break;
        }
      }
    }
  }

  // find var within namespace

  for (int i=0; i<ns->localVars.length; i++) {
    if (wcsncmp(searchName, ns->localVars.vars[i].name, searchNameLength) == 0) {
      return &ns->localVars.vars[i];
    }
  }

  return NULL;
}

void varListInit(VarList *list) {
  list->length = 0;
  list->allocatedLength = 0;
  list->vars = NULL;
}

void varListFreeContents(VarList *list) {
  if (list != NULL) {
    if (list->vars != NULL) {
      for (int i=0; i<list->length; i++) {
        varFreeContents(&list->vars[i]);
      }
      free(list->vars);
      list->vars = NULL;
      list->length = 0;
      list->allocatedLength = 0;
    }
  }
}

RetVal tryAppendVar(VarList *list, Var var, Error* error) {

  RetVal ret;

  if (list->vars == NULL) {
    uint64_t len = 16;
    tryMalloc(list->vars, len * sizeof(Var), "Var array");
    list->allocatedLength = len;
  }
  else if (list->length == list->allocatedLength) {
    uint64_t newAllocatedLength = list->allocatedLength * 2;

    Var* resizedVars = realloc(list->vars, newAllocatedLength * sizeof(Var));
    if (resizedVars == NULL) {
      ret = memoryError(error, "realloc Var array");
      goto failure;
    }

    list->allocatedLength = newAllocatedLength;
    list->vars = resizedVars;
  }

  list->vars[list->length] = var;
  list->length = list->length + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

//RetVal tryDefVar(FormAnalyzer *analyzer, wchar_t *symbolName, uint64_t symbolNameLength, Form *value, Error *error) {
//  RetVal ret;
//
// //   these get cleaned up on failure
//  Var createdVar;
//  Form *copiedValue;
//
//  Var *resolvedVar = resolveVar(analyzer, symbolName, symbolNameLength);
//  if (resolvedVar == NULL) {
//    Namespace *ns = analyzer->currentNamespace;
//    throws(tryVarInit(ns->name, symbolName, &createdVar, error));
//    throws(tryFormDeepCopy(value, &createdVar.value, error));
//    throws(tryAppendVar(&ns->localVars, createdVar, error));
//  }
//  else if (resolvedVar->value == NULL) {
//    throws(tryFormDeepCopy(value, &resolvedVar->value, error));
//  }
//  else {
//    throws(tryFormDeepCopy(value, &copiedValue, error));
//    formFree(resolvedVar->value);
//    resolvedVar->value = copiedValue;
//    copiedValue = NULL; // now part of resolvedVar
//  }
//
//  return R_SUCCESS;
//
//  failure:
//  varFreeContents(&createdVar);
//  if (copiedValue != NULL) {
//    formFree(copiedValue);
//  }
//  return ret;
//}

RetVal tryNamespaceMake(wchar_t *name, uint64_t length, Namespace **ptr , Error *error) {

  RetVal ret;

  Namespace *ns;
  tryMalloc(ns, sizeof(Namespace), "Namespace");

  ns->name = NULL;
  ns->importedVars = NULL;
  ns->numImportedVars = 0;

  varListInit(&ns->localVars);

  throws(tryCopyText(name, &ns->name, length, error));

  *ptr = ns;
  return R_SUCCESS;

  failure:
  if (ns != NULL) {
    free(ns);
  }
  return ret;
}

void namespaceFree(Namespace *ns) {
  if (ns != NULL) {
    free(ns->name);
    free(ns->importedVars);
    varListFreeContents(&ns->localVars);
    free(ns);
  }
}

RetVal tryNamespacesMake(Namespaces **ptr, Error *error) {

  RetVal ret;

  Namespaces *analyzer;
  tryMalloc(analyzer, sizeof(Namespaces), "FormAnalyzer");

  Namespace *userNs;
  throws(tryNamespaceMake(L"user", wcslen(L"user"), &userNs, error));

  analyzer->currentNamespace = userNs;
  analyzer->namespaces = userNs;
  analyzer->numNamespaces = 1;

  *ptr = analyzer;
  return R_SUCCESS;

  failure:
  if (analyzer != NULL) {
    free(analyzer);
  }
  return ret;
}

void analyzerFree(Namespaces *analyzer) {
  if (analyzer != NULL) {
    analyzer->currentNamespace = NULL;
    if (analyzer->namespaces != NULL) {
      for (int i=0; i<analyzer->numNamespaces; i++) {
        namespaceFree(&analyzer->namespaces[i]);
      }
      free(analyzer->namespaces);
    }
    free(analyzer);
  }
}

/*
 * gc/runtime implementation
 */

typedef struct Fn {
  uint16_t numArgs;
  uint16_t numConstants;
  Value *constants;
  Code code;
} Fn;

typedef struct String {
  uint64_t length;
  wchar_t *value;
} String;

typedef struct GC {

  uint64_t allocatedFnSpace;
  uint64_t usedFnSpace;
  Fn *fns;

  uint64_t allocatedStringSpace;
  uint64_t usedStringSpace;
  String *strings;

} GC;

void GCInit(GC *gc) {

  gc->usedFnSpace = 0;
  gc->allocatedFnSpace = 0;
  gc->fns = NULL;

  gc->allocatedStringSpace = 0;
  gc->usedStringSpace = 0;
  gc->strings = NULL;
}

RetVal tryAllocateFn(GC *gc, Fn fn, Value *value, Error *error) {
  RetVal ret;

  if (gc->fns == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->fns, len * sizeof(Fn), "Fn array");
    gc->allocatedFnSpace = len;
  }
  else if (gc->usedFnSpace == gc->allocatedFnSpace) {
    uint64_t newAllocatedLength = gc->allocatedFnSpace * 2;

    Fn* resizedFns = realloc(gc->fns, newAllocatedLength);
    if (resizedFns == NULL) {
      ret = memoryError(error, "realloc Fn array");
      goto failure;
    }

    gc->allocatedFnSpace = newAllocatedLength;
    gc->fns = resizedFns;
  }

  uint64_t index = gc->usedFnSpace;
  gc->fns[index] = fn;
  gc->usedFnSpace = index + 1;

  value->type = VT_FN;
  value->value = index;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryDerefFn(GC *gc, Value value, Fn *fn, Error *error) {
  RetVal ret;

  if (gc->usedFnSpace <= value.value) {
    throwInternalError(error, "fn reference points to fn that does not exist");
  }

  *fn = gc->fns[value.value];
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryAllocateString(GC *gc, String str, Value *value, Error *error) {
  RetVal ret;

  if (gc->strings == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->strings, len * sizeof(String), "String array");
    gc->allocatedStringSpace = len;
  }
  else if (gc->usedStringSpace == gc->allocatedStringSpace) {
    uint64_t newAllocatedLength = gc->allocatedStringSpace * 2;

    String* resizedStrings = realloc(gc->strings, newAllocatedLength);
    if (resizedStrings == NULL) {
      ret = memoryError(error, "realloc String array");
      goto failure;
    }

    gc->allocatedStringSpace = newAllocatedLength;
    gc->strings = resizedStrings;
  }

  uint64_t index = gc->usedStringSpace;
  gc->strings[index] = str;
  gc->usedStringSpace = index + 1;

  value->type = VT_STR;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryDerefString(GC *gc, Value value, String *str, Error *error) {
  RetVal ret;

  if (gc->usedStringSpace <= value.value) {
    throwInternalError(error, "str reference points to str that does not exist");
  }

  *str = gc->strings[value.value];
  return R_SUCCESS;

  failure:
  return ret;
}

typedef struct VM {
  // TODO: we don't support vars or define yet
  GC gc;
} VM;

RetVal tryVMMake(VM_t *ptr, Error *error) {
  RetVal ret;

  VM *vm;
  tryMalloc(vm, sizeof(VM), "VM");

  GCInit(&vm->gc);

  *ptr = vm;
  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFree(VM *vm) {
  if (vm != NULL) {
    free(vm);
  }
}

typedef struct OpStack {
  Value *stack;
  uint64_t maxDepth;
  uint64_t usedDepth;
} OpStack;

RetVal tryOpStackInitContents(OpStack *stack, uint64_t maxDepth, Error *error) {
  RetVal ret;

  stack->maxDepth = maxDepth;
  stack->usedDepth = 0;
  tryMalloc(stack->stack, sizeof(Value) * maxDepth, "Value array");
  return R_SUCCESS;

  failure:
    return ret;
}

void opStackFreeContents(OpStack *stack) {
  if (stack != NULL) {
    stack->maxDepth = 0;
    stack->usedDepth = 0;
    if (stack->stack != NULL) {
      free(stack->stack);
      stack->stack = NULL;
    }
  }
}

RetVal tryOpStackPush(OpStack *stack, Value v, Error *error) {
  RetVal ret;

  if (stack->maxDepth == stack->usedDepth + 1) {
    throwRuntimeError(error, "cannot allocate op stack greater than max %llu", stack->maxDepth);
  }

  stack->stack[stack->usedDepth] = v;
  stack->usedDepth = stack->usedDepth + 1;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryOpStackPop(OpStack *stack, Value *ptr, Error *error) {

  RetVal ret;

  if (stack->usedDepth == 0) {
    throwRuntimeError(error, "cannot pop from empty op stack")
  }

  stack->usedDepth = stack->usedDepth - 1;
  *ptr = stack->stack[stack->usedDepth];
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCodeDeepCopy(Code *from, Code *to, Error *error) {
  RetVal ret;

  to->numLocals = from->numLocals;
  to->maxOperandStackSize = from->maxOperandStackSize;
  to->codeLength = from->codeLength;

  tryMalloc(to->code, sizeof(uint8_t) * to->codeLength, "Code array");
  memcpy(to->code, from->code, to->codeLength);

  to->hasSourceTable = from->hasSourceTable;

  if (to->hasSourceTable) {
    to->sourceTable.fileNameLength = from->sourceTable.fileNameLength;
    throws(tryCopyText(from->sourceTable.fileName, &to->sourceTable.fileName, to->sourceTable.fileNameLength, error));

    to->sourceTable.numLineNumbers = from->sourceTable.numLineNumbers;
    tryMalloc(to->sourceTable.lineNumbers, sizeof(LineNumber) * to->sourceTable.numLineNumbers, "LineNumber array");
  }

  return R_SUCCESS;

  failure:
    codeFreeContents(to);
    return ret;
}


RetVal tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value **ptr, Error *error);

void _fnFreeContents(Fn *fn) {
  if (fn != NULL) {
    fn->numArgs = 0;

    fn->numConstants = 0;
    if (fn->constants != NULL) {
      free(fn->constants);
      fn->constants = NULL;
    }

    codeFreeContents(&fn->code);
  }
}

RetVal tryFnHydrate(VM *vm, FnConstant *fnConst, Value *value, Error *error) {
  RetVal ret;

  // cleanup on failure

  Fn fn;
  fn.numArgs = fnConst->numArgs;

  fn.numConstants = fnConst->numConstants;
  tryMalloc(fn.constants, sizeof(Value) * fn.numConstants, "Value array");
  throws(tryHydrateConstants(vm, fn.numConstants, fnConst->constants, &fn.constants, error));

  throws(tryCodeDeepCopy(&fnConst->code, &fn.code, error));

  throws(tryAllocateFn(&vm->gc, fn, value, error));

  return R_SUCCESS;

  failure:
    _fnFreeContents(&fn);
    return ret;
}

RetVal tryStringHydrate(VM *vm, StringConstant strConst, Value *value, Error *error) {
  RetVal ret;

  String str;
  str.length = strConst.length;
  throws(tryCopyText(strConst.value, &str.value, str.length, error));
  throws(tryAllocateString(&vm->gc, str, value, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVarRefHydrate(VM *vm, VarRefConstant varRefConst, Value *value, Error *error) {
  RetVal ret;

  String str;
  str.length = varRefConst.nameLength;
  throws(tryCopyText(varRefConst.name, &str.value, str.length, error));
  throws(tryAllocateString(&vm->gc, str, value, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value **ptr, Error *error) {
  RetVal ret;

  // clean up on failure
  Value *values;

  tryMalloc(values, sizeof(Value) * numConstants, "Value array");

  for (uint16_t i=0; i<numConstants; i++) {

    Constant c = constants[i];
    Value v;

    switch (c.type) {
      case CT_BOOL:
        v.type = VT_BOOL;
        v.value = c.boolean;
        break;
      case CT_INT:
        v.type = VT_UINT;
        v.value = c.integer;
        break;
      case CT_NIL:
        v.type = VT_NIL;
        v.value = 0;
        break;
      case CT_FN:
        throws(tryFnHydrate(vm, &c.function, &v, error));
        break;
      case CT_VAR_REF:
        throws(tryVarRefHydrate(vm, c.varRef, &v, error));
        break;
      case CT_STR:
        throws(tryStringHydrate(vm, c.string, &v, error));
        break;
      case CT_NONE:
        throwInternalError(error, "invalid constant");
        break;
    }

    values[i] = v;
  }

  *ptr = values;
  return R_SUCCESS;

  failure:
    if (values != NULL) {
      free(values);
    }
    return ret;
}

typedef struct Frame Frame;

typedef struct Frame {
  Frame *parent;
  uint16_t numConstants; // TODO: make a verifier so we can check these bounds at load time rather than compile time
  Value *constants;
  Code code;
  Value *locals;
  OpStack *opStack;
  Value result;
} Frame;

RetVal tryFrameEval(VM *vm, Frame *frame, Error *error);

RetVal tryInvoke(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value invocable;
  throws(tryOpStackPop(frame->opStack, &invocable, error));

  if (invocable.type != VT_FN) {
    throwRuntimeError(error, "cannot invoke this as a function!");
  }

  Fn fn;
  throws(tryDerefFn(&vm->gc, invocable, &fn, error));

  // clean up on return
  Value *locals = NULL;
  OpStack opStack;

  tryMalloc(locals, sizeof(Value) * fn.code.numLocals, "Value array");
  tryOpStackInitContents(&opStack, fn.code.maxOperandStackSize, error);

  // pop args and set as locals, reversing the order
  for (uint16_t i=0; i<fn.numArgs; i++) {
    Value arg;
    throws(tryOpStackPop(frame->opStack, &arg, error));
    uint16_t idx = fn.numArgs - (uint16_t)1 - i;
    locals[idx] = arg;
  }

  Frame child;
  child.parent = frame;
  child.numConstants = fn.numConstants;
  child.constants = fn.constants;
  child.code = fn.code;
  child.locals = locals;
  child.opStack = &opStack;
  child.result.type = VT_NIL;
  child.result.value = 0;

  throws(tryFrameEval(vm, &child, error));
  throws(tryOpStackPush(frame->opStack, child.result, error));

  free(child.locals);
  opStackFreeContents(&opStack);

  return R_SUCCESS;

  failure:
    free(child.locals);
    opStackFreeContents(&opStack);
    return ret;
}

RetVal tryFrameEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  bool returnFound = false;
  uint16_t pc = 0;

  while (!returnFound) {
    uint8_t inst = frame->code.code[pc];

    switch (inst) {

      case I_LOAD_CONST:  { // (8), index (16) | (-> value)

        uint16_t constantIndex = (frame->code.code[pc + 1] << 8) | frame->code.code[pc + 2];
        Value constant = frame->constants[constantIndex];
        throws(tryOpStackPush(frame->opStack, constant, error));

        pc = pc + 3;
        break;
      }

      case I_LOAD_LOCAL:  { // (8), index  (16) | (-> value)
        uint16_t localIndex = *((uint16_t *)(frame->code.code + pc + 1));
        Value v = frame->locals[localIndex];
        throws(tryOpStackPush(frame->opStack, v, error));

        pc = pc + 3;
        break;
      }

      case I_STORE_LOCAL: { // (8), index  (16) | (objectref ->)
        uint16_t localIndex = *((uint16_t *)(frame->code.code + pc + 1));
        Value v;
        throws(tryOpStackPop(frame->opStack, &v, error));
        frame->locals[localIndex] = v;

        pc = pc + 3;
        break;
      }

      case I_INVOKE_DYN: {      // (8)              | (objectref, args... -> ...)
        throws(tryInvoke(vm, frame, error));
        pc = pc + 1;
        break;
      }
      case I_RET: {         // (8)              | (objectref ->)
        Value v;
        throws(tryOpStackPop(frame->opStack, &v, error));
        frame->result = v;
        returnFound = true;
        break;
      }
      case I_CMP: {         // (8)              | (a, b -> 0 | 1)
        Value a, b;
        throws(tryOpStackPop(frame->opStack, &a, error));
        throws(tryOpStackPop(frame->opStack, &b, error));

        Value c;
        c.type = VT_BOOL;
        c.value = false;

        if (a.type == b.type && a.value == b.value) {
          // NOTE: doesn't do equivalence on heap objects, reference identity compared only
          c.value = true;
        }

        throws(tryOpStackPush(frame->opStack, c, error));

        pc = pc + 1;
        break;
      }
      case I_JMP: {         // (8), offset (16) | (->)
        uint16_t newPc = *((uint16_t *)(frame->code.code + pc + 1));
        pc = newPc;
        break;
      }
      case I_JMP_IF: {      // (8), offset (16) | (value ->)

        Value test;
        throws(tryOpStackPop(frame->opStack, &test, error));

        bool truthy;
        if (test.type == VT_BOOL) {
          truthy = test.value > 0;
        }
        else if (test.type == VT_UINT) {
          truthy = test.value > 0;
        }
        else if (test.type == VT_NIL) {
          truthy = false;
        }
        else {
          throwRuntimeError(error, "unhandled truth");
        }

        if (truthy) {
          uint16_t newPc = *((uint16_t *) (frame->code.code + pc + 1));
          pc = newPc;
        }

        break;
      }
      case I_JMP_IF_NOT: {      // (8), offset (16) | (value ->)

        Value test;
        throws(tryOpStackPop(frame->opStack, &test, error));

        bool truthy;
        if (test.type == VT_BOOL) {
          truthy = test.value > 0;
        }
        else if (test.type == VT_UINT) {
          truthy = test.value > 0;
        }
        else if (test.type == VT_NIL) {
          truthy = false;
        }
        else {
          throwRuntimeError(error, "unhandled truth");
        }

        if (!truthy) {
          uint16_t newPc = *((uint16_t *) (frame->code.code + pc + 1));
          pc = newPc;
        }

        break;
      }
      case I_ADD: {        // (8)              | (a, b -> c)
        Value a, b;
        throws(tryOpStackPop(frame->opStack, &a, error));
        throws(tryOpStackPop(frame->opStack, &b, error));

        if (a.type != VT_UINT || b.type != VT_UINT) {
          throwRuntimeError(error, "can only add two integers");
        }

        Value c;
        c.type = VT_UINT;
        c.value = a.value + b.value;

        throws(tryOpStackPush(frame->opStack, c, error));

        pc = pc + 1;
        break;
      }
      case I_DEF_VAR: {     // (8)  offset (16)  | (value ->)

        Value value;
        throws(tryOpStackPop(frame->opStack, &value, error));

        uint16_t constantIndex = (frame->code.code[pc + 1] << 8) | frame->code.code[pc + 2];
        Value varConst = frame->constants[constantIndex];
//        constant.

        // resolveVar();

        pc = pc + 3;
        break;
      }
      case I_LOAD_VAR: {    // (8)              | (name -> value)
        throwRuntimeError(error, "load var unimplemented");
      }

        // requires garbage collection
      case I_NEW:           // (8), objlen (16) | (-> objectref)
      case I_GET_FIELD:     // (8), index  (16) | (objectref -> value)
      case I_SET_FIELD:     // (8), index  (16) | (objectref, value ->)
      case I_NEW_ARRAY:     // (8), objlen (16) | (arraylen -> objectref)
      case I_LOAD_ARRAY:    // (8)              | (objectref, index -> value)
      case I_STORE_ARRAY:   // (8)              | (objectref, index, value ->)
      throwRuntimeError(error, "instruction unimplemented");
        break;
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

typedef struct TopLevelFrame {
  uint16_t numConstants;
  Value *constants;
  Code code;
  uint16_t numLocals;
  Value *locals;
  OpStack opStack;
} TopLevelFrame;

void topLevelFrameFreeContents(TopLevelFrame *topLevel);

RetVal tryTopLevelFrameLoad(VM *vm, TopLevelFrame *topLevel, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  // hydrate all constant values referenced by the code

  topLevel->numConstants = codeUnit->numConstants;
  tryMalloc(topLevel->constants, sizeof(Value) * topLevel->numConstants, "Value array");
  throws(tryHydrateConstants(vm, codeUnit->numConstants, codeUnit->constants, &topLevel->constants, error));

  // deep-copy the code

  throws(tryCodeDeepCopy(&codeUnit->code, &topLevel->code, error));

  // allocate frame space

  topLevel->numLocals = codeUnit->code.numLocals;
  tryMalloc(topLevel->locals, sizeof(Value) * topLevel->numLocals, "Value array for locals");
  throws(tryOpStackInitContents(&topLevel->opStack, codeUnit->code.maxOperandStackSize, error));
  return R_SUCCESS;

  failure:
  topLevelFrameFreeContents(topLevel);
  return ret;
}

void topLevelFrameFreeContents(TopLevelFrame *topLevel) {
  if (topLevel != NULL) {

    topLevel->numConstants = 0;
    if (topLevel->constants != NULL) {
      free(topLevel->constants);
      topLevel->constants = NULL;
    }

    codeFreeContents(&topLevel->code);

    topLevel->numLocals = 0;
    if (topLevel->locals != NULL) {
      free(topLevel->locals);
      topLevel->locals = NULL;
    }

    opStackFreeContents(&topLevel->opStack);
  }
}

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, Value *result, Error *error) {

  RetVal ret;

  TopLevelFrame *topLevel;
  tryMalloc(topLevel, sizeof(TopLevelFrame), "TopLevelFrame")
  throws(tryTopLevelFrameLoad(vm, topLevel, codeUnit, error));

  Frame frame;
  frame.parent = NULL;
  frame.numConstants = topLevel->numConstants;
  frame.constants = topLevel->constants;
  frame.code = topLevel->code;
  frame.locals = topLevel->locals;
  frame.opStack = &topLevel->opStack;
  frame.result.type = VT_NIL;
  frame.result.value = 0;

  throws(tryFrameEval(vm, &frame, error));

  topLevelFrameFreeContents(topLevel);

  *result = frame.result;
  return R_SUCCESS;

  failure:
    if (topLevel != NULL) {
      topLevelFrameFreeContents(topLevel);
    }
    return ret;
}

/*
 * Here begin scratch thoughts that are probably mostly invalid by the time anyone reads them.
 */

// hold a table of Value references to functions and constants defined internally within this function
// this way this function can be hydrated from a CodeUnit once, and have all its inner functions
// hydrated once as well.

// functions should not have 'inner' functions. The bytecode format should require that all functions definittions
// be hoisted to the top level. code that references functions expliclitly by value, and not indirectly, can use
// the mechanism below:

// going from a CodeUnit to a hydrated representation of a function should not result in a code rewrite
// you can avoid a rewrite by having the bytecode say "load the value of this function reference", where the
// reference is mapped in the header of the Fn to an actual object reference to the function in question. This can
// be populated at the time that the Fn is loaded as a value for the first time.

// a big part of my confusion has been conflating function loading with function invocation/execution

// It must be a property of functions that they retain references to the values they explicitly reference
// beyond a given function call, otherwise all these values have to be re-hydrated from the CodeUnit every time...

//};

// there are three concepts I'd like to separate:
// 1. the CodeUnit that is generated for a function
//    - this includes the actual instructions
//    - this also includes source metadata and constants
//    - this should live on the heap, and be gc-able
// 2. the actual value that can be used to invoke that function
//    - this should really be a singleton
//    - this should live on the heap, and be gc-able
//    - this should hold an object reference to its CodeUnit to prevent it from being gc-ed too soon
// 3. the way this value is discovered by code that wants to invoke it
//    - a literal function reference, returned as a value from another function call
//    - a local
//    - a var
// and yet...
// it would be much easier to duplicate the constants into the functions themselves where needed

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Value Spec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

// In this machine, all values are represented by a 64-bit word.
//
// The leftmost 3 bits are used to encode the following types. The remaining 61
// bits are interpreted on a per-type basis.
//
// :unsigned-int - an overflowable unsigned integer
// :bool         - 0 for false, 1 for true
// :nil          - constant, always 0
// :char         - the lowest 32 bits represent a UTF-16 character
// :object       - interpreted as an unsigned integer, the value is a pointer
//                 offset to dynamically-allocated memory on the heap.
//
// Objects on the heap are represented this way:
//
// [56 bits - total object size in words][8 bits - specific type of object] [...]
//
// Here are the object types:
//
// :char-array (0)   - The first word is an unsigned integer containing the
//                     number of characters in the string. Each subsequent word
//                     contains up to two UTF-16 characters, one in the higher 32
//                     bits and one in the lower 32 bits. This is an optimization
//                     for representing Strings.
//
// :object-array (1) - The first word is an unsigned integer containing the
//                     number of characters in the string. Each subsequent word
//                     contains a value.
//
// :record-type (2)  - Describes the names of the fields in a record, and their
//                     indexes.
//
// :record (3)       - The first word is the Value that describes the record-type
//                     for a record. The rest of the words are values that
//                     describe the record's fields.
//
// :function (5)     - The first word is the number of arguments the function
//                     accepts. The second word is a string value that is the
//                     source code for the function. The remainder of the words
//                     are instructions, which are represented as word singles
//                     or triples: main instruction, arg hint, arg
//

// TODO: how do we represent lists in the virtual machine? are they implemented on-top of arrays/records? (YES)
// TODO: is a function reference represented differently from a lambda?
// TODO: there appears to be no meaningful difference between args and locals within the vm, they are all just locals
// though captured variables are different, since they may get boxed as part of a lambda


/*
 * When the vm is asked to evaluate code, it always evaluates it in the context of a specific namespace. The symbols
 * defined in this namespace form the basis of the evaluation environment. Also part of the environment is
 * pre-allocated space for the locals the code will introduce. Last, the code will require an operand stack to do
 * anything useful, so one is pre-allocated based on the stack usage the code requires.
 *
 * When the vm evaluates code, it expects that the top-level expression will terminate in a `ret` instruction so it
 * will know the result of the expression it evaluated. The compiler must detect that a form is a top-level
 * form and generate this extra instruction as needed.
 */

//F_CONST,   -> I_LOAD_*
//F_IF,      -> I_JMP_IF
//F_LET,     -> I_STORE_LOCAL
//F_DEF,     -> I_DEFINE
//F_ENV_REF, -> I_LOAD_LOCAL
//F_VAR_REF, -> I_LOAD_VAR
//F_FN,     -> defined fns
//F_BUILTIN,
//F_FN_CALL -> I_INVOKE_DYN

/*
 * tryVMEval(vm, code, &result); // the result is a Value, which the caller can then introspect
 *
 * load code into VM as temporary zero-argument function within current namespace
 * invoke function
 * destroy function
 */

/*
 * the virtual machine at minimum needs to have a main method that accepts a file with bytecode as input
 * this file needs a format, which could just be a sequence of sexprs where each one represents either:
 *
 * - a function definition, containing instructions
 * - an expression, which is just a bag of instructions with no
 */

/* notes transcription:
 *
 * What does a VM need to be useful?
 *
 * - It needs to expose access to the runtime from executing bytecode:
 *
 */

/*
 * So I've been thinking about how to proceed with this VM business...
 *
 * I'm thinking of:
 * - doing the full virtual machine
 * - except just using the current ast as an input
 * - and also doing the vm as a repl, where basically you call an API in a single-threaded fashion to evaluate code
 * - there will be a true, command-line repl, but there will also be an API-based repl for the compiler to use
 * - if using the AST verbatim as the instruction format becomes hard, then I'll know how the bytecode should be different
 *   from the AST.
 * - other than the input format, everything else about the internals of the vm should be as if bytecode was fed in
 *   instead of AST forms. this includes the registers, the operations on the registers, and the runtime.
 *
 * *PERHAPS*: I should just do the fucking instruction set and be done with it
 */
