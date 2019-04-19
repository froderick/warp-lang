  #include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>
#include "vm.h"
#include "utils.h"

/*
 * CodeUnit init/free functions
 */

void lineNumberInitContents(LineNumber *n) {
  n->lineNumber = 0;
  n->startInstructionIndex = 0;
}

void sourceTableInitContents(SourceTable *t) {
  t->lineNumbers = NULL;
  t->numLineNumbers = 0;
  textInitContents(&t->fileName);
}

void sourceTableFreeContents(SourceTable *t) {
  textFreeContents(&t->fileName);
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

void constantMetaPropertyInit(ConstantMetaProperty *p) {
  p->keyIndex = 0;
  p->valueIndex = 0;
}

void constantMetaInit(ConstantMeta *c) {
  c->numProperties = 0;
  c->properties = NULL;
}

void constantMetaFreeContents(ConstantMeta *c) {
  if (c != NULL) {
    free(c->properties);
    c->properties = NULL;
    c->numProperties = 0;
  }
}

void constantFnInitContents(FnConstant *fnConst) {
  fnConst->fnId = 0;
  fnConst->hasName = 0;
  textInitContents(&fnConst->name);
  fnConst->bindingSlotIndex = 0;
  fnConst->numArgs = 0;
  fnConst->usesVarArgs = false;
  fnConst->numConstants = 0;
  fnConst->numCaptures = 0;
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
      case CT_SYMBOL:
        c->symbol.length = 0;
        if (c->symbol.value != NULL) {
          free(c->symbol.value);
          c->symbol.value = NULL;
        }
        break;
      case CT_KEYWORD:
        c->keyword.length = 0;
        if (c->keyword.value != NULL) {
          free(c->keyword.value);
          c->keyword.value = NULL;
        }
        break;
      case CT_LIST:
        c->list.length = 0;
        if (c->list.constants != NULL) {
          free(c->list.constants);
          c->list.constants = NULL;
        }
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

void exFrameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

void exFrameFreeContents(VMExceptionFrame *f) {
  if (f != NULL) {
    textFreeContents(&f->functionName);
    f->lineNumber = 0;
    textFreeContents(&f->fileName);
  }
}

void framesInitContents(VMExceptionFrames *f) {
  f->length = 0;
  f->elements = NULL;
}

void framesFreeContents(VMExceptionFrames *f) {
  if (f != NULL) {
    if (f->elements != NULL) {
      for (uint64_t i = 0; i < f->length; i++) {
        VMExceptionFrame *fr = &f->elements[i];
        exFrameFreeContents(fr);
      }
      free(f->elements);
      f->length = 0;
      f->elements = NULL;
    }
  }
}

void _frameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

void exceptionInitContents(VMException *e) {
  textInitContents(&e->message);
  framesInitContents(&e->frames);
}

void exceptionFreeContents(VMException *e) {
  if (e != NULL) {
    textFreeContents(&e->message);
    framesFreeContents(&e->frames);
  }
}

void evalResultInitContents(VMEvalResult *r) {
  r->type = RT_NONE;
}

void evalResultFreeContents(VMEvalResult *r) {
  if (r != NULL) {
    switch (r->type){
      case RT_RESULT:
        exprFreeContents(&r->result);
        break;
      case RT_EXCEPTION:
        exceptionFreeContents(&r->exception);
        break;
      case RT_NONE:
        break;
    }
  }
}

/*
 * VM Data Structures
 */

/*
 * In this machine, all values are represented by a 64-bit word.
 * The leftmost 4 bits are used to encode the following types. The remaining 60
 * bits are interpreted on a per-type basis.
 * :unsigned-int - an overflowable unsigned integer
 * :bool         - 0 for false, 1 for true
 * :nil          - constant, always 0
 * :char         - the lowest 32 bits represent a UTF-16 character
 * :object       - interpreted as an unsigned integer, the value is a pointer
 *                 offset to dynamically-allocated memory on the heap.
 */
 typedef enum ValueType {
   VT_NIL,
   VT_UINT,
   VT_BOOL,
   VT_FN,
   VT_STR,
   VT_SYMBOL,
   VT_KEYWORD,
   VT_LIST,
   VT_CLOSURE,
   VT_CFN,
//  VT_OBJECT,
//  VT_CHAR,
} ValueType;

typedef struct Value {
  ValueType type : 4;
  uint64_t value : 60;
} Value;

/*
 * This is the first field inside all heap objects. It must come first so that the GC can
 * scan through the heap, for which it needs to determine object sizes and object types.
 */
typedef struct ObjectHeader {
  ValueType type : 4;
  size_t size : 60;
  Value metadata;
} ObjectHeader;

typedef struct ExecFrame *ExecFrame_t;
typedef RetVal (*CFnInvoke) (VM_t vm, ExecFrame_t frame, Error *error);

typedef struct CFn {
  ObjectHeader header;

  uint64_t nameLength;
  size_t nameOffset;
  CFnInvoke ptr;
  uint16_t numArgs;
  bool usesVarArgs;
} CFn;

wchar_t* cFnName(CFn *fn) { return (void*)fn + fn->nameOffset; }

typedef struct Fn {
  ObjectHeader header;

  bool hasName;
  uint64_t nameLength;
  size_t nameOffset;
  uint16_t bindingSlotIndex;

  uint16_t numCaptures;
  uint16_t numArgs;
  bool usesVarArgs;

  uint16_t numConstants;
  size_t constantsOffset;

  uint16_t numLocals;           // the number of local bindings this code unit uses
  uint64_t maxOperandStackSize; // the maximum number of items this code pushes onto the operand stack at one time
  uint64_t codeLength;          // the number of bytes in this code block
  size_t codeOffset;

  bool hasSourceTable;
  uint64_t sourceFileNameLength;
  size_t sourceFileNameOffset;
  uint64_t numLineNumbers;
  size_t lineNumbersOffset;

} Fn;

wchar_t* fnName(Fn *fn) { return (void*)fn + fn->nameOffset; }
Value* fnConstants(Fn *fn) { return (void*)fn + fn->constantsOffset; }
uint8_t* fnCode(Fn *fn) { return (void*)fn + fn->codeOffset; }
wchar_t* fnSourceFileName(Fn *fn) { return (void*)fn + fn->sourceFileNameOffset; }
LineNumber* fnLineNumbers(Fn *fn) { return (void*)fn + fn->lineNumbersOffset; }

typedef struct Closure {
  ObjectHeader header;

  Value fn;
  uint16_t numCaptures;
  size_t capturesOffset;
} Closure;

Value* closureCaptures(Closure *closure) { return (void*)closure + closure->capturesOffset; }

typedef struct String {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
} String;

wchar_t* stringValue(String *x) { return (void*)x + x->valueOffset; }

typedef struct Symbol {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
} Symbol;

wchar_t* symbolValue(Symbol *x) { return (void*)x + x->valueOffset; }

typedef struct Keyword {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
} Keyword;

wchar_t* keywordValue(Keyword *x) { return (void*)x + x->valueOffset; }

typedef struct Cons {
  ObjectHeader header;

  Value value;
  Value next; // this must be a Cons, or Nil
} Cons;

typedef struct GC {

  // the total memory allocated
  uint64_t heapMemorySize;
  void *heapMemory;

  // the actual heaps
  uint64_t heapSize;
  void *heapA; // the first half of the memory
  void *heapB; // the second half of the memory

  // the current heap
  void *currentHeap; // the heap to use for allocation
  void *currentHeapEnd; // the heap to use for allocation
  void *allocPtr;    // the offset within the heap to use for allocation

} GC;

// namespaces

typedef struct Var {
  wchar_t *namespace;
  wchar_t *name;
  Value value;
  bool isMacro;
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

// instruction definitions

typedef RetVal (*TryEval) (struct VM *vm, ExecFrame_t frame, Error *error);

typedef struct Inst {
  const char *name;
  void (*print)(int *i, const char* name, uint8_t *code);
  TryEval tryEval;
} Inst;

typedef struct InstTable {
  uint8_t numInstructions;
  Inst instructions[256];
} InstTable;

// value type definitions

typedef struct Invocable {
  Value ref;
  Value fnRef;
  Fn *fn;
  bool hasClosure;
  Closure *closure;
} Invocable;

typedef RetVal (*TryRelocateChildren)(VM_t vm, void *oldHeap, void *obj, Error *error);
typedef RetVal (*TryPrn)(VM_t vm, Value result, Expr *expr, Error *error);

typedef struct ValueTypeInfo {
  const char *name;
  bool isHeapObject;
  bool (*isTruthy)(Value value);
  TryRelocateChildren tryRelocateChildren;
  TryPrn tryPrn;
} ValueTypeInfo;

typedef struct ValueTypeTable {
  uint8_t numValueTypes;
  ValueTypeInfo valueTypes[256];
} ValueTypeTable;

// vm state

typedef struct VM {
  GC gc;
  Namespaces namespaces;
  InstTable instTable;
  ValueTypeTable valueTypeTable;
} VM;

// frames

typedef struct OpStack {
  Value *stack;
  uint64_t maxDepth;
  uint64_t usedDepth;
} OpStack;

/*
 * Common value factories
 */

Value nil() {
  Value v;
  v.type = VT_NIL;
  v.value = 0;
  return v;
}

void objectHeaderInitContents(ObjectHeader *h) {
  h->type = VT_NIL;
  h->size = 0;
  h->metadata = nil();
}

/*
 * The ExecFrame and operations it supports
 */

RetVal readInstruction(ExecFrame_t frame, uint8_t *ptr, Error *error);
RetVal readIndex(ExecFrame_t frame, uint16_t *ptr, Error *error);
RetVal setPc(ExecFrame_t frame, uint16_t newPc, Error *error);
RetVal getConst(ExecFrame_t frame, uint16_t constantIndex, Value *ptr, Error *error);
RetVal getLocal(ExecFrame_t frame, uint16_t localIndex, Value *ptr, Error *error);
RetVal setLocal(ExecFrame_t frame, uint16_t localIndex, Value value, Error *error);
RetVal getLocalRef(ExecFrame_t frame, uint16_t localIndex, Value **ptr, Error *error);
uint16_t numLocals(ExecFrame_t frame);
uint64_t numOperands(ExecFrame_t frame);
RetVal getOperandRef(ExecFrame_t frame, uint64_t opIndex, Value **ptr, Error *error);
uint16_t pushOperand(ExecFrame_t frame, Value value, Error *error);
uint16_t popOperand(ExecFrame_t frame, Value *value, Error *error);
Value getFnRef(ExecFrame_t frame);
RetVal setFnRef(VM *vm, ExecFrame_t frame, Value value, Error *error);

bool hasResult(ExecFrame_t frame);
bool hasParent(ExecFrame_t frame);
RetVal getParent(ExecFrame_t frame, ExecFrame_t *ptr, Error *error);
RetVal setResult(ExecFrame_t frame, Value result, Error *error);
RetVal getResult(ExecFrame_t frame, Value *ptr, Error *error);

typedef struct ExceptionHandler {
  uint16_t jumpAddress;
  uint16_t localIndex;
} ExceptionHandler;

bool hasHandler(ExecFrame_t frame);
RetVal getHandler(ExecFrame_t frame, ExceptionHandler *ptr, Error *error);
void setHandler(ExecFrame_t frame, ExceptionHandler handler);
void clearHandler(ExecFrame_t frame);

bool hasFnName(ExecFrame_t frame);
RetVal getFnName(ExecFrame_t frame, Text *name, Error *error);

bool hasSourceTable(ExecFrame_t frame);
bool getLineNumber(ExecFrame_t frame, uint64_t *lineNumber);
bool getFileName(ExecFrame_t frame, Text *fileName);

bool hasException(ExecFrame_t frame);
void setException(ExecFrame_t frame, VMException e);
RetVal getException(ExecFrame_t frame, VMException *e, Error *error);

RetVal pushFrame(VM *vm, ExecFrame_t *frame, Value newFn, Error *error);
RetVal replaceFrame(VM *vm, ExecFrame_t frame, Value newFn, Error *error);
void popFrame(ExecFrame_t frame);

/*
 * value type protocols
 */

const char* getValueTypeName(VM *vm, uint8_t type) {
  return vm->valueTypeTable.valueTypes[type].name;
}

bool isHeapObject(VM *vm, Value value) {
  return vm->valueTypeTable.valueTypes[value.type].isHeapObject;
}

bool isTruthy(VM *vm, Value value) {
  return vm->valueTypeTable.valueTypes[value.type].isTruthy(value);
}

RetVal tryRelocateChildren(VM *vm, ValueType type, void *oldHeap, void *obj, Error *error) {
  TryRelocateChildren relocate = vm->valueTypeTable.valueTypes[type].tryRelocateChildren;
  if (relocate != NULL) {
    return relocate(vm, oldHeap, obj, error);
  }
  else {
    return R_SUCCESS;
  }
}

/*
 * NEW alloc/gc impl
 *
 * super useful: http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm
 */

void GCFreeContents(GC *gc) {
  free(gc->heapMemory);
  gc->heapMemory = NULL;
  gc->heapA = NULL;
  gc->heapB = NULL;
  gc->currentHeap = NULL;
  gc->allocPtr = NULL;
}

void GCInitContents(GC *gc) {
  gc->heapMemorySize = 0;
  gc->heapMemory = NULL;
  gc->heapSize = 0;
  gc->heapA = NULL;
  gc->heapB = NULL;
  gc->currentHeap = NULL;
  gc->allocPtr = NULL;
}

RetVal tryGCInitContents(GC *gc, uint64_t maxHeapSize, Error *error) {
  RetVal ret;

  GCInitContents(gc);

  gc->heapSize = maxHeapSize;
  gc->heapMemorySize = gc->heapSize * 2;

  tryMalloc(gc->heapMemory, gc->heapMemorySize, "GC memory");
  memset(gc->heapMemory, 0, gc->heapMemorySize);

  gc->heapA = gc->heapMemory;
  gc->heapB = gc->heapA + gc->heapSize;
  gc->currentHeap = gc->heapA;
  gc->currentHeapEnd = gc->currentHeap + gc->heapSize;
  gc->allocPtr = gc->currentHeap;

  return R_SUCCESS;
  failure:
    return ret;
}

void collect(VM *vm, ExecFrame_t frame, Error *error);

#define R_OOM 1

/*
 * Allocates, returns R_OOM if allocation fails. Doesn't attempt collection.
 */
int _alloc(GC *gc, uint64_t length, void **ptr, uint64_t *offset) {

  /*
   * each object must be at least the size of a pointer, so we can replace it with a
   * forwarding pointer during gc
   */
  if (length < 8) {
    length = 8;
  }

  if (gc->allocPtr + length < gc->currentHeapEnd) {
    *ptr = gc->allocPtr;
    *offset = gc->allocPtr - gc->currentHeap;
    gc->allocPtr += length;
    return R_SUCCESS;
  }
  else {
    return R_OOM;
  }
}

/*
 * Allocates, attempts collection if allocation fails.
 */
RetVal alloc(VM *vm, ExecFrame_t frame, uint64_t length, void **ptr, uint64_t *offset, Error *error) {
  RetVal ret;

  int success = _alloc(&vm->gc, length, ptr, offset);

  if (success == R_OOM) {
    collect(vm, frame, error);

    success = _alloc(&vm->gc, length, ptr, offset);

    if (success == R_OOM) {
      throwRuntimeError(error, "out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal deref(GC *gc, void **ptr, uint64_t offset, Error *error) {
  RetVal ret;

  if (offset > gc->heapSize) {
    throwRuntimeError(error, "invalid memory address");
  }

  *ptr = gc->currentHeap + offset;

  return R_SUCCESS;
  failure:
  return ret;
}

bool inCurrentHeap(GC *gc, void *ptr) {
  return gc->currentHeap <= ptr && ptr < gc->currentHeapEnd;
}

RetVal relocate(VM *vm, void *oldHeap, Value *value, Error *error) {
  RetVal ret;

  if (!isHeapObject(vm, *value)) {
    // only relocate heap objects
    return R_SUCCESS;
  }

  GC *gc = &vm->gc;

  void *ptr = NULL;
  if (value->value > gc->heapSize) {
    throwRuntimeError(error, "invalid memory address");
  }
  ptr = oldHeap + value->value;

  void **forwardPtr = ptr;
  if (inCurrentHeap(gc, *forwardPtr)) {
    value->value = *forwardPtr - gc->currentHeap;
  }
  else {
    size_t size = ((ObjectHeader*)ptr)->size;

    void *newPtr = NULL;
    uint64_t offset = 0;

    if (_alloc(gc, size, &newPtr, &offset) == R_OOM) {
      throwRuntimeError(error, "out of memory, cannot allocate %lu bytes mid-gc", size);
    }

    memcpy(newPtr, ptr, size);
    value->value = newPtr - gc->currentHeap;

    *forwardPtr = newPtr;
  }

  return R_SUCCESS;
  failure:
    return ret;
}

uint64_t now() {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC_RAW, &now);
  uint64_t millis = now.tv_nsec / 1000000;
  return millis;
}

void collect(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint64_t oldHeapUsed = vm->gc.allocPtr - vm->gc.currentHeap;

  uint64_t start = now();
  printf("gc: starting, %" PRIu64 " bytes used\n", oldHeapUsed);

  // flip heaps
  void *oldHeap = vm->gc.currentHeap;
  if (oldHeap == vm->gc.heapA) {
    vm->gc.currentHeap = vm->gc.heapB;
  }
  else {
    vm->gc.currentHeap = vm->gc.heapA;
  }
  vm->gc.currentHeapEnd = vm->gc.currentHeap + vm->gc.heapSize;
  vm->gc.allocPtr = vm->gc.currentHeap;

  memset(vm->gc.currentHeap, 0, vm->gc.heapSize);

  // relocate var roots
  for (uint64_t i=0; i<vm->namespaces.numNamespaces; i++) {
    Namespace *ns = &vm->namespaces.namespaces[i];
    for (uint64_t j=0; j<ns->localVars.length; j++) {
      Var *var = &ns->localVars.vars[j];
      throws(relocate(vm, oldHeap, &var->value, error));
    }
  }

  // relocate call stack roots
  ExecFrame_t current = frame;
  while (true) {

    // relocate fnRef
    {
      Value oldFnRef = getFnRef(current);
      Value newFnRef = oldFnRef;
      throws(relocate(vm, oldHeap, &newFnRef, error));

      if (oldFnRef.value != newFnRef.value) {
        throws(setFnRef(vm, current, newFnRef, error));
      }
    }

    uint16_t locals = numLocals(current);
    for (uint16_t i=0; i<locals; i++) {

      Value *val = NULL;
      throws(getLocalRef(current, i, &val, error));

      throws(relocate(vm, oldHeap, val, error));
    }

    uint64_t operands = numOperands(current);
    for (uint64_t i=0; i<operands; i++) {

      Value *val = NULL;
      throws(getOperandRef(current, i, &val, error));

      throws(relocate(vm, oldHeap, val, error));
    }

    if (!hasParent(current)) {
      break;
    }
    else {
      throws(getParent(current, &current, error) != R_SUCCESS);
    }
  }

  void *scanptr = vm->gc.currentHeap;

  // relocate all the objects this object references
  while (scanptr < vm->gc.allocPtr) {
    ObjectHeader *header = scanptr;
    throws(tryRelocateChildren(vm, header->type, oldHeap, scanptr, error));
    scanptr += header->size;
  }

  uint64_t newHeapUsed = vm->gc.allocPtr - vm->gc.currentHeap;
  uint64_t sizeRecovered = oldHeapUsed - newHeapUsed;
  uint64_t end = now();
  uint64_t duration = end - start;

  printf("gc: completed, %" PRIu64 " bytes recovered, %" PRIu64 " bytes used, took %" PRIu64 "ms\n", sizeRecovered, newHeapUsed, duration);

  return;

  failure:
    printf("collect() failed, terminating process :)\n");
    exit(-1);
}

/*
 * Loading Constants as Values
 */

RetVal tryHydrateConstant(VM *vm, Value *alreadyHydratedConstants, Constant c, Value *ptr, uint16_t constantIndex, Error *error);

RetVal _tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value *values, Error *error);

void fnInitContents(Fn *fn) {

  objectHeaderInitContents(&fn->header);

  fn->hasName = false;
  fn->nameLength = 0;
  fn->nameOffset = 0;
  fn->bindingSlotIndex = 0;

  fn->numCaptures = 0;
  fn->numArgs = 0;
  fn->usesVarArgs = false;
  fn->numConstants = 0;
  fn->constantsOffset = 0;

  fn->numLocals = 0;
  fn->maxOperandStackSize = 0;
  fn->codeLength = 0;
  fn->codeOffset = 0;

  fn->hasSourceTable = false;
  fn->sourceFileNameLength = 0;
  fn->sourceFileNameOffset = 0;
  fn->numLineNumbers = 0;
  fn->lineNumbersOffset = 0;
}

RetVal tryFnHydrate(VM *vm, FnConstant *fnConst, Value *value, Error *error) {
  RetVal ret;

  // cleanup on failure
  Fn *fn = NULL;

  size_t nameSize = (fnConst->name.length + 1) * sizeof(wchar_t);
  size_t constantsSize = fnConst->numConstants * sizeof(Value);
  size_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
  size_t sourceFileNameSize = (fnConst->code.sourceTable.fileName.length + 1) * sizeof(wchar_t);
  size_t lineNumbersSize = fnConst->code.sourceTable.numLineNumbers * sizeof(LineNumber);

  size_t fnSize = sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize + lineNumbersSize;

  uint64_t offset = 0;
  if (_alloc(&vm->gc, fnSize, (void*)&fn, &offset) == R_OOM) {
    throwRuntimeError(error, "out of memory, failed to allocate fn constant");
  }

  value->type = VT_FN;
  value->value = offset;

  fnInitContents(fn);

  fn->header.type = VT_FN;
  fn->header.size = fnSize;

  fn->nameOffset           = sizeof(Fn);
  fn->constantsOffset      = sizeof(Fn) + nameSize;
  fn->codeOffset           = sizeof(Fn) + nameSize + constantsSize;
  fn->sourceFileNameOffset = sizeof(Fn) + nameSize + constantsSize + codeSize;
  fn->lineNumbersOffset    = sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize;

  fn->hasName = fnConst->hasName;
  if (fn->hasName) {
    fn->nameLength = fnConst->name.length;
    size_t copySize = fnConst->name.length * sizeof(wchar_t);

    memcpy(fnName(fn), fnConst->name.value, copySize);
    fnName(fn)[fn->nameLength] = L'\0';

    fn->bindingSlotIndex = fnConst->bindingSlotIndex;
  }

  fn->numCaptures = fnConst->numCaptures;
  fn->numArgs = fnConst->numArgs;
  fn->usesVarArgs = fnConst->usesVarArgs;

  fn->numConstants = fnConst->numConstants;
  throws(_tryHydrateConstants(vm, fn->numConstants, fnConst->constants, fnConstants(fn), error));

  fn->numLocals = fnConst->code.numLocals;
  fn->maxOperandStackSize = fnConst->code.maxOperandStackSize;

  {
    fn->codeLength = fnConst->code.codeLength;
    memcpy(fnCode(fn), fnConst->code.code, codeSize);
  }

  fn->hasSourceTable = fnConst->code.hasSourceTable;
  if (fn->hasSourceTable) {

      fn->sourceFileNameLength = fnConst->code.sourceTable.fileName.length;
      size_t copySize = fnConst->code.sourceTable.fileName.length * sizeof(wchar_t);

      memcpy(fnSourceFileName(fn), fnConst->code.sourceTable.fileName.value, copySize);
      fnSourceFileName(fn)[fn->sourceFileNameLength] = L'\0';

      fn->numLineNumbers = fnConst->code.sourceTable.numLineNumbers;
      memcpy(fnLineNumbers(fn), fnConst->code.sourceTable.lineNumbers, lineNumbersSize);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void stringInitContents(String *s) {
  objectHeaderInitContents(&s->header);
  s->length = 0;
  s->valueOffset = 0;
}

RetVal _tryStringHydrate(VM *vm, wchar_t *text, uint64_t length, Value *value, Error *error) {
  RetVal ret;

  String *str = NULL;

  size_t textSize = (length + 1) * sizeof(wchar_t);
  size_t strSize = sizeof(String) + textSize;

  uint64_t offset = 0;

  if (_alloc(&vm->gc, strSize, (void*)&str, &offset) == R_OOM) {
    throwRuntimeError(error, "out of memory, failed to allocate string constant: %ls", text);
  }

  value->type = VT_STR;
  value->value = offset;

  stringInitContents(str);
  str->header.type = VT_STR;
  str->header.size = strSize;
  str->length = length;

  str->valueOffset = sizeof(String);
  memcpy(stringValue(str), text, length * sizeof(wchar_t));
  stringValue(str)[length] = L'\0';

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryStringHydrate(VM *vm, StringConstant strConst, Value *value, Error *error) {
  RetVal ret;

  throws(_tryStringHydrate(vm, strConst.value, strConst.length, value, error));
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVarRefHydrate(VM *vm, VarRefConstant varRefConst, Value *value, Error *error) {
  RetVal ret;

  throws(_tryStringHydrate(vm, varRefConst.name, varRefConst.nameLength, value, error));
  return R_SUCCESS;

  failure:
  return ret;
}

void symbolInitContents(Symbol *s) {
  objectHeaderInitContents(&s->header);
  s->length = 0;
  s->valueOffset = 0;
}

RetVal trySymbolHydrate(VM *vm, SymbolConstant symConst, Value *value, Error *error) {
  RetVal ret;

  Symbol *sym = NULL;

  size_t textSize = (symConst.length + 1) * sizeof(wchar_t);
  size_t size = sizeof(Symbol) + textSize;

  uint64_t offset = 0;
  if (_alloc(&vm->gc, size, (void*)&sym, &offset) == R_OOM) {
    throwRuntimeError(error, "out of memory, failed to allocate symbol: %ls", symConst.value);
  }

  value->type = VT_SYMBOL;
  value->value = offset;

  symbolInitContents(sym);
  sym->header.type = VT_SYMBOL;
  sym->header.size = size;
  sym->length = symConst.length;

  sym->valueOffset = sizeof(Symbol);
  memcpy(symbolValue(sym), symConst.value, sym->length * sizeof(wchar_t));
  symbolValue(sym)[sym->length] = L'\0';

  return R_SUCCESS;

  failure:
  return ret;
}

void keywordInitContents(Keyword *k) {
  objectHeaderInitContents(&k->header);
  k->length = 0;
  k->valueOffset = 0;
}

RetVal tryKeywordHydrate(VM *vm, KeywordConstant kwConst, Value *value, Error *error) {
  RetVal ret;

  Keyword *kw = NULL;

  size_t textSize = (kwConst.length + 1) * sizeof(wchar_t);
  size_t size = sizeof(Keyword) + textSize;

  uint64_t offset = 0;
  if (_alloc(&vm->gc, size, (void*)&kw, &offset) == R_OOM) {
    throwRuntimeError(error, "out of memory, failed to allocate keyword: %ls", kwConst.value);
  }

  value->type = VT_KEYWORD;
  value->value = offset;

  keywordInitContents(kw);
  kw->header.type = VT_KEYWORD;
  kw->header.size = size;
  kw->length = kwConst.length;

  kw->valueOffset = sizeof(Keyword);
  memcpy(keywordValue(kw), kwConst.value, kw->length * sizeof(wchar_t));
  keywordValue(kw)[kw->length] = L'\0';

  return R_SUCCESS;

  failure:
  return ret;
}

void consInitContents(Cons *c) {
  objectHeaderInitContents(&c->header);
  c->value = nil();
  c->next = nil();
}

RetVal _tryAllocateCons(VM *vm, Value value, Value next, Value meta, Value *ptr, Error *error) {
  RetVal ret;

  if (next.type != VT_NIL && next.type != VT_LIST) {
    throwRuntimeError(error, "a Cons next must be nil or a list: %u", next.type);
  }

  Cons *cons = NULL;

  size_t size = sizeof(Cons);

  uint64_t offset = 0;
  if (_alloc(&vm->gc, size, (void*)&cons, &offset) == R_OOM) {
    throwRuntimeError(error, "out of memory, failed to allocate cons");
  }

  ptr->type = VT_LIST;
  ptr->value = offset;

  consInitContents(cons);
  cons->header.type = VT_LIST;
  cons->header.size = size;
  cons->header.metadata = meta;
  cons->value = value;
  cons->next = next;

  return R_SUCCESS;
  failure:
  return ret;
}

/*
 * alreadyHydratedConstants is a pointer to the array of all values materialized for the current fn or codeunit, so far.
 * it is included so that references to already-hydrated values can be resolved by constant index.
 */
RetVal tryListHydrate(VM *vm, Value *alreadyHydratedConstants, ListConstant listConst, Value *value, Error *error) {
  RetVal ret;

  // build up meta property list with conses
  Value meta = nil();

  for (uint64_t i=0; i<listConst.meta.numProperties; i++) {
    ConstantMetaProperty *p = &listConst.meta.properties[i];
    throws(_tryAllocateCons(vm, alreadyHydratedConstants[p->valueIndex], meta, nil(), &meta, error));
    throws(_tryAllocateCons(vm, alreadyHydratedConstants[p->keyIndex], meta, nil(), &meta, error));
  }

  // build up list with conses, each cons gets the same meta
  Value seq = nil();

  for (uint16_t i = 0; i < listConst.length; i++) {

    uint16_t listConstEnd = listConst.length - 1;
    uint16_t valueIndex = listConst.constants[listConstEnd - i];

    throws(_tryAllocateCons(vm, alreadyHydratedConstants[valueIndex], seq, meta, &seq, error));
  }

  *value = seq;
  return R_SUCCESS;

  failure:
  return ret;
}

// TODO: I had a thought, can we get rid of CodeUnit entirely and just replace it with FnConstant?
// TODO: I had another thought, can we get rid of the nested graph of constants and flatten it entirely?

RetVal tryHydrateConstant(VM *vm, Value *alreadyHydratedConstants, Constant c, Value *ptr, uint16_t constantIndex,
    Error *error) {
  RetVal ret;

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
    case CT_SYMBOL:
      throws(trySymbolHydrate(vm, c.symbol, &v, error));
      break;
    case CT_KEYWORD:
      throws(tryKeywordHydrate(vm, c.keyword, &v, error));
      break;
    case CT_LIST:
      throws(tryListHydrate(vm, alreadyHydratedConstants, c.list, &v, error));
      break;
    case CT_NONE:
    default:
      throwInternalError(error, "invalid constant: %u", c.type);
  }

  *ptr = v;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal _tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value *values, Error *error) {
  RetVal ret;

  for (uint16_t i=0; i<numConstants; i++) {

    Constant c = constants[i];
    Value v;

    throws(tryHydrateConstant(vm, values, c, &v, i, error));

    values[i] = v;
  }

  return R_SUCCESS;
  failure:
    return ret;
}

/*
 * Create a reader representation of a Value (an Expr).
 *
 * Some representations are approximate and cannot be round-tripped through eval, such as functions and closures.
 */
RetVal tryVMPrn(VM *vm, Value result, Expr *expr, Error *error) {
  RetVal ret;

  exprInitContents(expr);

  TryPrn prn = vm->valueTypeTable.valueTypes[result.type].tryPrn;
  throws(prn(vm, result, expr, error));

  return R_SUCCESS;

  failure:
    exprFreeContents(expr);
    return ret;
}

/*
 * Managing namespaces of vars
 */

void varFree(Var *var);

void varInitContents(Var *var) {
  var->namespace = NULL;
  var->name = NULL;
  var->value.type = VT_NIL;
  var->value.value = 0;
  var->isMacro = false;
}

RetVal tryVarInit(wchar_t *namespace, wchar_t *name, uint64_t symbolNameLength, Value value, Var *var, Error *error) {
  RetVal ret;

  varInitContents(var);

  throws(tryCopyText(namespace, &var->namespace, wcslen(namespace), error));
  throws(tryCopyText(name, &var->name, symbolNameLength, error));
  var->value = value;

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

bool resolveVar(Namespaces *analyzer, wchar_t *symbolName, uint64_t symbolNameLength, Var **var) {

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
    if (wcscmp(searchName, ns->localVars.vars[i].name) == 0) {
      *var = &ns->localVars.vars[i];
      return true;
    }
  }

  return false;
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

RetVal tryDefVar(Namespaces *namespaces, wchar_t *symbolName, uint64_t symbolNameLength, Value value, Error *error) {
  RetVal ret;

  // gets cleaned up on failure
  Var createdVar;
  varInitContents(&createdVar);

  Var *resolvedVar = NULL;
  if (!resolveVar(namespaces, symbolName, symbolNameLength, &resolvedVar)) {
    Namespace *ns = namespaces->currentNamespace;
    throws(tryVarInit(ns->name, symbolName, symbolNameLength, value, &createdVar, error));
    throws(tryAppendVar(&ns->localVars, createdVar, error));
  }
  else {
    resolvedVar->value = value;
  }

  return R_SUCCESS;

  failure:
  varFreeContents(&createdVar);
  return ret;
}

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

void namespaceFreeContents(Namespace *ns) {
  if (ns != NULL) {
    free(ns->name);
    free(ns->importedVars);
    varListFreeContents(&ns->localVars);
  }
}

RetVal tryNamespacesInitContents(Namespaces *namespaces, Error *error) {

  RetVal ret;

  Namespace *userNs;
  throws(tryNamespaceMake(L"user", wcslen(L"user"), &userNs, error));

  namespaces->currentNamespace = userNs;
  namespaces->namespaces = userNs;
  namespaces->numNamespaces = 1;

  return R_SUCCESS;

  failure:
  return ret;
}

void namespacesFreeContents(Namespaces *namespaces) {
  if (namespaces != NULL) {
    namespaces->currentNamespace = NULL;
    if (namespaces->namespaces != NULL) {
      for (int i=0; i<namespaces->numNamespaces; i++) {
        namespaceFreeContents(&namespaces->namespaces[i]);
      }
      free(namespaces->namespaces);
    }
  }
}

RetVal tryAllocateCons(VM *vm, ExecFrame_t frame, Value value, Value next, Value *ptr, Error *error) {
  RetVal ret;

  if (next.type != VT_NIL && next.type != VT_LIST) {
    throwRuntimeError(error, "a Cons next must be nil or a list: %s", getValueTypeName(vm, next.type));
  }

  Cons *cons = NULL;

  size_t size = sizeof(Cons);

  uint64_t offset = 0;
  throws(alloc(vm, frame, size, (void*)&cons, &offset, error));

  ptr->type = VT_LIST;
  ptr->value = offset;

  consInitContents(cons);
  cons->header.type = VT_LIST;
  cons->header.size = size;
  cons->value = value;
  cons->next = next;

  return R_SUCCESS;
  failure:
  return ret;
}

/*
 * Instruction Definitions
 */

// (8), typeIndex (16) | (-> value)
RetVal tryLoadConstEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex;
  Value constant;

  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &constant, error));
  throws(pushOperand(frame, constant, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), typeIndex (16) | (-> value)
RetVal tryLoadLocalEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t localIndex;
  Value v;

  throws(readIndex(frame, &localIndex, error));
  throws(getLocal(frame, localIndex, &v, error));
  throws(pushOperand(frame, v, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), typeIndex  (16) | (objectref ->)
RetVal tryStoreLocalEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t localIndex;
  Value v;

  throws(readIndex(frame, &localIndex, error));
  throws(popOperand(frame, &v, error));
  throws(setLocal(frame, localIndex, v, error));

  return R_SUCCESS;

  failure:
  return ret;
}

/*
 * TODO: let's make a separate heap (perm-gen) for compiled, loaded code
 * - this would mean that we can depend on the locations of functions not changing due to garbage collection
 *   while regular instructions within a function are being executed.
 * - we would garbage collect functions whenever the space is exhausted, which would be caused by attempting to
 *   load new code. we'd start from the roots like normal gc, just with different semantics
 * - the only time functions would get moved is when loading more code, which triggers gc.
 * - this lets us write vm code that keeps direct pointers to hydrated compiled code
 *
 * - see https://www.quora.com/What-are-some-best-practices-in-using-the-Java-8-JVM-Metaspace
 */

void invocableInitContents(Invocable *i) {
  i->ref = nil();    // the reference to the initially invoked value (could be closure or fn)
  i->fnRef = nil();  // always points to the actual fn
  i->fn = NULL;
  i->hasClosure = false;
  i->closure = NULL;
}

RetVal tryPopInvocable(VM *vm, Value pop, Invocable *invocable, Error *error) {
  RetVal ret;

  invocableInitContents(invocable);

  invocable->ref = pop;
  invocable->fnRef = pop;

  switch (invocable->fnRef.type) {
    case VT_FN: {

      throws(deref(&vm->gc, (void*)&invocable->fn, invocable->fnRef.value, error));

      invocable->hasClosure = false;
      invocable->closure = NULL;
      break;
    }
    case VT_CLOSURE: {

      throws(deref(&vm->gc, (void*)&invocable->closure, invocable->fnRef.value, error));

      invocable->hasClosure = true;
      invocable->fnRef = invocable->closure->fn;
      throws(deref(&vm->gc, (void*)&invocable->fn, invocable->closure->fn.value, error));
      break;
    }
    case VT_CFN: {
      throws(deref(&vm->gc, (void*)&invocable->fn, invocable->fnRef.value, error));
    }
    default:
      // fail: not all values are invocable
      throwRuntimeError(error, "cannot invoke this value type as a function: %s",
          getValueTypeName(vm, invocable->fnRef.type));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

//* - vm honors var-arg flag
//*   - sees var-arg flag on invocable
//*   - pops all static arguments into local slot
//*   - pops number indicating number of extra arguments
//*   - we *aways* pass the number of arguments
//*   - pops number of extra arguments into a list, sets as final argument in local slot

RetVal tryPreprocessArguments(VM *vm, ExecFrame_t parent, uint16_t numArgs, bool usesVarArgs, Error *error) {

  RetVal ret;

  Value numArgsSupplied;
  throws(popOperand(parent, &numArgsSupplied, error));

  if (numArgsSupplied.type != VT_UINT) {
    throwRuntimeError(error, "first op stack value must be number of arguments supplied: %s",
                      getValueTypeName(vm, numArgsSupplied.type));
  }

  if (!usesVarArgs) {
    if (numArgsSupplied.value != numArgs) {
      throwRuntimeError(error, "required arguments not supplied, expected %u but got %" PRIu64, numArgs,
          numArgsSupplied.value);
    }
  }
  else {
    if (numArgsSupplied.value > numArgs) {

      // push empty varargs sequence
      throws(pushOperand(parent, nil(), error));

      // read the extra args into that sequence, push it back on the stack
      uint16_t numVarArgs = (numArgsSupplied.value - numArgs) + 1;
      for (uint16_t i = 0; i < numVarArgs; i++) {

        Value seq = nil();

        // may gc, so has to happen before we pop anything off the stack
        throws(tryAllocateCons(vm, parent, nil(), nil(), &seq, error));

        // gc possibility over, so pop sequence and arg from the stack and set them on cons
        Cons *cons = NULL;
        throws(deref(&vm->gc, (void*)&cons, seq.value, error));
        throws(popOperand(parent, &cons->next, error));
        throws(popOperand(parent, &cons->value, error));

        // put the new sequence back on the stack
        throws(pushOperand(parent, seq, error));
      }
    }
    else if (numArgsSupplied.value == numArgs) {
      // wrap the last arg in a list

      Value seq = nil();

      //may gc, so has to happen before we pop anything off the stack
      throws(tryAllocateCons(vm, parent, nil(), nil(), &seq, error));

      // gc possibility over, so pop arg from the stack and it on cons
      Cons *cons = NULL;
      throws(deref(&vm->gc, (void*)&cons, seq.value, error));
      throws(popOperand(parent, &cons->value, error));

      // put the one-element sequence back on the stack
      throws(pushOperand(parent, seq, error));
    }
    else if (numArgsSupplied.value == numArgs - 1) {
      // the final argument will be an empty list, make sure the list is present on the op stack
      throws(pushOperand(parent, nil(), error));
    }
    else {
      throwRuntimeError(error, "required arguments not supplied, expected %u or more arguments but got %" PRIu64,
          numArgs - 1, numArgsSupplied.value);
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryInvokePopulateLocals(VM *vm, ExecFrame_t parent, ExecFrame_t child, Invocable invocable, Error *error) {
  RetVal ret;

  throws(tryPreprocessArguments(vm, parent, invocable.fn->numArgs, invocable.fn->usesVarArgs, error));

  for (uint16_t i = 0; i < invocable.fn->numArgs; i++) {
    Value arg;
    throws(popOperand(parent, &arg, error));

    uint16_t idx = invocable.fn->numArgs - (1 + i);
    throws(setLocal(child, idx, arg, error));
  }

  if (invocable.fn->numCaptures > 0) {

    if (!invocable.hasClosure) {
      throwRuntimeError(error, "cannot invoke this fn without a closure, it captures variables: %u", invocable.fn->numCaptures);
    }
    if (invocable.closure->numCaptures < invocable.fn->numCaptures) {
      throwRuntimeError(error, "closure does not have enough captured variables: %u", invocable.closure->numCaptures);
    }

    uint16_t nextLocalIdx = invocable.fn->numArgs;
    for (uint16_t i=0; i<invocable.fn->numCaptures; i++) {
      throws(setLocal(child, nextLocalIdx, closureCaptures(invocable.closure)[i], error));
      nextLocalIdx = nextLocalIdx + 1;
    }
  }

  if (invocable.fn->hasName) {
    throws(setLocal(child, invocable.fn->bindingSlotIndex, invocable.ref, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryInvokeCFn(VM *vm, ExecFrame_t frame, Value cFn, Error *error) {
  RetVal ret;

  CFn *fn = NULL;
  throws(deref(&vm->gc, (void*)&fn, cFn.value, error));
  throws(tryPreprocessArguments(vm, frame, fn->numArgs, fn->usesVarArgs, error));
  throws(fn->ptr(vm, frame, error));

  return R_SUCCESS;
  failure:
  return ret;
}

// (8)              | (objectref, args... -> ...)
RetVal tryInvokeDynEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  // for cleanup on failure
  bool pushed = false;

  Value pop = nil();
  throws(popOperand(frame, &pop, error));

  if (pop.type == VT_CFN) {
    throws(tryInvokeCFn(vm, frame, pop, error));
  }
  else {
    Invocable invocable;
    throws(tryPopInvocable(vm, pop, &invocable, error));

    throws(pushFrame(vm, &frame, invocable.fnRef, error));
    pushed = true;

    ExecFrame_t parent;
    throws(getParent(frame, &parent, error));

    throws(tryInvokePopulateLocals(vm, parent, frame, invocable, error));
  }

  return R_SUCCESS;

  failure:
    if (pushed) {
      popFrame(frame);
    }
    return ret;
}

/*
 * tail calls basically don't execute any code, they just re-use an existing stack frame
 * and set up a different function and arguments, reallocate locals as needed, and reset the pc to 0.
 * then the execution starts all over again in the same frame
 *
 * TODO: How do I write tests for this?
 * `(fib 1000000)` smashes my default stack size (8mb on macos) without tail calls, so it validates that the
 * tail calls are being used, but it is slow (2s) and I'm impatient.
 */

// (8)              | (objectref, args... -> ...)
RetVal tryInvokeDynTailEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value pop = nil();
  throws(popOperand(frame, &pop, error));

  if (pop.type == VT_CFN) {
    throws(tryInvokeCFn(vm, frame, pop, error));
  }
  else {
    Invocable invocable;
    throws(tryPopInvocable(vm, pop, &invocable, error));

    throws(replaceFrame(vm, frame, invocable.fnRef, error));
    throws(tryInvokePopulateLocals(vm, frame, frame, invocable, error));
  }

  return R_SUCCESS;
  failure:
    return ret;
}

// (8)              | (objectref ->)
RetVal tryRetEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value v;
  throws(popOperand(frame, &v, error));
  throws(setResult(frame, v, error));
  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> 0 | 1)
RetVal tryCmpEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &a, error));
  throws(popOperand(frame, &b, error));

  Value c;
  c.type = VT_BOOL;
  c.value = false;

  if (a.type == b.type && a.value == b.value) {
    // NOTE: doesn't do equivalence on heap objects, reference identity compared only
    c.value = true;
  }

  throws(pushOperand(frame, c, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (->)
RetVal tryJmpEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t newPc;
  throws(readIndex(frame, &newPc, error));
  throws(setPc(frame, newPc, error));
  return R_SUCCESS;

  failure:
    return ret;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value test;
  throws(popOperand(frame, &test, error));

  bool truthy = isTruthy(vm, test);

  uint16_t newPc;
  throws(readIndex(frame, &newPc, error));
  if (truthy) {
    throws(setPc(frame, newPc, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfNotEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value test;
  throws(popOperand(frame, &test, error));

  bool truthy = isTruthy(vm, test);

  uint16_t newPc;
  throws(readIndex(frame, &newPc, error));
  if (!truthy) {
    throws(setPc(frame, newPc, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal tryAddEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &b, error));
  throws(popOperand(frame, &a, error));

  if (a.type != VT_UINT) {
    throwRuntimeError(error, "can only add integers: %s", getValueTypeName(vm, a.type));
  }
  if (b.type != VT_UINT) {
    throwRuntimeError(error, "can only add integers: %s", getValueTypeName(vm, b.type));
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value + b.value;

  throws(pushOperand(frame, c, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal trySubEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &b, error));
  throws(popOperand(frame, &a, error));

  if (a.type != VT_UINT) {
    throwRuntimeError(error, "can only subtract integers: %s", getValueTypeName(vm, a.type));
  }
  if (b.type != VT_UINT) {
    throwRuntimeError(error, "can only subtract integers: %s", getValueTypeName(vm, b.type));
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value - b.value;

  throws(pushOperand(frame, c, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16)  | (value ->)
RetVal tryDefVarEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  uint16_t constantIndex;
  Value varName;

  throws(popOperand(frame, &value, error));
  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &varName, error));

  String *str = NULL;
  throws(deref(&vm->gc, (void*)&str, varName.value, error));

  throws(tryDefVar(&vm->namespaces, stringValue(str), str->length, value, error));

  // define always returns nil
  Value result;
  result.type = VT_NIL;
  result.value = 0;
  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset 16  | (-> value)
RetVal tryLoadVarEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex;
  Value varName;

  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &varName, error));

  if (varName.type != VT_STR) {
    throwRuntimeError(error, "expected a string: %s", getValueTypeName(vm, varName.type));
  }

  String *str = NULL;
  throws(deref(&vm->gc, (void*)&str, varName.value, error));

  if (wcscmp(stringValue(str), L"second") == 0) {
      printf("what\n");
  }

  Var *var = NULL;
  if (!resolveVar(&vm->namespaces, stringValue(str), str->length, &var)) {
    // fail: not all vars exist
    throwRuntimeError(error, "no such var found: '%ls'", stringValue(str));
  }
  else {
    throws(pushOperand(frame, var->value, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

void closureInitContents(Closure *cl) {
  objectHeaderInitContents(&cl->header);
  cl->fn = nil();
  cl->numCaptures = 0;
  cl->capturesOffset = 0;
}

// (8), offset (16) | (captures... -> value)
RetVal tryLoadClosureEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex;
  Value fnValue;

  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &fnValue, error));

  if (fnValue.type != VT_FN) {
    throwRuntimeError(error, "cannot create a closure from this value type: %s",
        getValueTypeName(vm, fnValue.type));
  }

  Fn *fn;
  throws(deref(&vm->gc, (void*)&fn, fnValue.value, error));

  Closure *closure = NULL;

  size_t capturesSize = fn->numCaptures * sizeof(Value);
  size_t clSize = sizeof(Closure) + capturesSize;

  uint64_t offset = 0;
  throws(alloc(vm, frame, clSize, (void*)&closure, &offset, error));

  Value closureValue;
  closureValue.type = VT_CLOSURE;
  closureValue.value = offset;

  closureInitContents(closure);
  closure->header.type = VT_CLOSURE;
  closure->header.size = clSize;
  closure->fn = fnValue;
  closure->numCaptures = fn->numCaptures;

  closure->capturesOffset = sizeof(Closure);

  // pop captures in reverse order, same as arguments
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    Value capture;
    throws(popOperand(frame, &capture, error));
    uint16_t idx = closure->numCaptures - (1 + i);
    closureCaptures(closure)[idx] = capture;
  }

  throws(pushOperand(frame, closureValue, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)        | (a, b -> b, a)
RetVal trySwapEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &a, error));
  throws(popOperand(frame, &b, error));

  throws(pushOperand(frame, a, error));
  throws(pushOperand(frame, b, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)        | (jumpAddr, handler ->)
RetVal trySetHandlerEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  ExceptionHandler handler;

  throws(readIndex(frame, &handler.jumpAddress, error));
  throws(readIndex(frame, &handler.localIndex, error));

  setHandler(frame, handler);

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)        | (->)
RetVal tryClearHandlerEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  clearHandler(frame);
  return R_SUCCESS;

  failure:
  return ret;
}

/* 1. It is ok to implement builtin c functions the same way we implement instructions:
 *    - one signature: RetVal doit(VM *vm, ExecFrame_t frame, Error *error);
 *    - each function must read its params from the op stack and write its return value to the opstack
 *    - each function must guard against triggering gc while it is running
 *
 * 2. It is ok to manually add builtin c functions and metadata to the registry in vm init code
 *    - this can be automated in the future
 *
 * 3. Builtins are defined entirely by the VM, and require no definitions in the standard library.
 *    - a VM always hydrates builtins on startup and defines them in vars before evaluating code
 *    - the 'builtin' special form goes away
 *
 * 4. Builtins are invocable just like any other function.
 *
 * //////////// old ideas /////////////
 *
 * - make a CFn object and add support for creating such objects programmatically within the VM
 * - when initializing the vm, for each builtin function, create the CFn object and define a var
 *   with that function's name, and CFn's value.
 * - make the CFn object invocable, such that it just stops short of invoking a specific c function and then bombs out
 * - make code to dynamically invoke such a c function based on the function's metadata stored in the CFn
 * - use a special comment format to identify functions and their arguments that can be called as CFn's, write code
 *   to scan c files as a part of the build process and generate metadata for the VM to load on start
 * // DECL_FN(name, arg1, arg2)
 *
 */

// (8),             | (x, seq -> newseq)
RetVal tryConsEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  // gc may occur, so allocate the cons first
  Value result = nil();
  throws(tryAllocateCons(vm, frame, nil(), nil(), &result, error));

  Value x, seq;
  throws(popOperand(frame, &seq, error));
  throws(popOperand(frame, &x, error));

  if (seq.type != VT_NIL && seq.type != VT_LIST) {
    throwRuntimeError(error, "cannot cons onto a value of type %s", getValueTypeName(vm, seq.type));
  }

  Cons *cons = NULL;
  throws(deref(&vm->gc, (void*)&cons, result.value, error));
  cons->value = x;
  cons->next = seq;

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> x)
RetVal tryFirstEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value seq;
  throws(popOperand(frame, &seq, error));

  Value result;

  if (seq.type == VT_NIL) {
    result = nil();
  }
  else if (seq.type == VT_LIST) {
    Cons *cons;
    throws(deref(&vm->gc, (void*)&cons, seq.value, error));
    result = cons->value;
  }
  else {
    throwRuntimeError(error, "cannot get first from a value of type %s", getValueTypeName(vm, seq.type));
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> seq)
RetVal tryRestEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value seq;
  throws(popOperand(frame, &seq, error));

  Value result;

  if (seq.type == VT_NIL) {
    result = nil();
  }
  else if (seq.type == VT_LIST) {

    Cons *cons;
    throws(deref(&vm->gc, (void*)&cons, seq.value, error));

    result = cons->next;
  }
  else {
    throwRuntimeError(error, "cannot get rest from a value of type %s", getValueTypeName(vm, seq.type));
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (name -> nil)
RetVal trySetMacroEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value strValue;
  throws(popOperand(frame, &strValue, error));

  wchar_t *sym = NULL;
  uint64_t symLength = 0;

  if (strValue.type == VT_STR) {
    String *str = NULL;
    throws(deref(&vm->gc, (void*)&str, strValue.value, error));
    sym = stringValue(str);
    symLength = str->length;
  }
  else if (strValue.type == VT_SYMBOL) {
    Symbol *s = NULL;
    throws(deref(&vm->gc, (void*)&s, strValue.value, error));
    sym = symbolValue(s);
    symLength = s->length;
  }
  else {
    throwRuntimeError(error, "only strings or symbols can identify vars: %s", getValueTypeName(vm, strValue.type));
  }

  Var *var;
  if (!resolveVar(&vm->namespaces, sym, symLength, &var)) {
    throwRuntimeError(error, "no such var exists: %ls", sym);
  }

  if (!var->isMacro) {
    if (var->value.type != VT_FN) {
      throwRuntimeError(error, "only vars referring to functions can be macros: %ls -> %s",
          sym,  getValueTypeName(vm, var->value.type));
    }
    var->isMacro = true;
  }

  throws(pushOperand(frame, nil(), error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (name -> bool)
RetVal tryGetMacroEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value strValue;
  throws(popOperand(frame, &strValue, error));

  wchar_t *sym = NULL;
  uint64_t symLength = 0;

  if (strValue.type == VT_STR) {
    String *str = NULL;
    throws(deref(&vm->gc, (void*)&str, strValue.value, error));
    sym = stringValue(str);
    symLength = str->length;
  }
  else if (strValue.type == VT_SYMBOL) {
    Symbol *s = NULL;
    throws(deref(&vm->gc, (void*)&s, strValue.value, error));
    sym = symbolValue(s);
    symLength = s->length;
  }
  else {
    throwRuntimeError(error, "only strings or symbols can identify vars: %s", getValueTypeName(vm, strValue.type));
  }

  String *str = NULL;
  throws(deref(&vm->gc, (void*)&str, strValue.value, error));

  Value result;
  result.type = VT_BOOL;
  result.value = false;

  Var *var;
  if (resolveVar(&vm->namespaces, sym, symLength, &var)) {
    result.value = var->isMacro;
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (name -> bool)
RetVal tryGCEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  collect(vm, frame, error);

  throws(pushOperand(frame, nil(), error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (value -> value)
RetVal tryGetTypeEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  throws(popOperand(frame, &value, error));

  Value typeId;
  typeId.type = VT_UINT;
  typeId.value = value.type;

  throws(pushOperand(frame, typeId, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVMPrn(VM *vm, Value result, Expr *expr, Error *error);

// (8),             | (value -> value)
RetVal tryPrnEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  throws(popOperand(frame, &value, error));

  Expr expr;
  throws(tryVMPrn(vm, value, &expr, error));
  throws(tryExprPrn(&expr, error));
  printf("\n");

  throws(pushOperand(frame, nil(), error));

  ret = R_SUCCESS;
  goto done;

  failure:
    goto done;

  done:
    exprFreeContents(&expr);
    return ret;
}

void printInst(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\n", *i, name);
}

void printInstAndIndex(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\t%u\n", *i, name, code[*i + 1] << 8 | code[*i + 2]);
  *i = *i + 2;
}

void printInstAndIndex2x(int *i, const char* name, uint8_t *code) {
  uint16_t index1 = code[*i + 1] << 8 | code[*i + 2];
  uint16_t index2 = code[*i + 3] << 8 | code[*i + 4];
  printf("%i:\t%s\t%u, %u\n", *i, name, index1, index2);
  *i = *i + 4;
}

void printUnknown(int *i, const char* name, uint8_t *code) {
  printf("%i:\t<UNKNOWN>/%u\n", *i, code[*i]);
}

InstTable instTableCreate() {
  InstTable table;

  // init table with blanks
  uint16_t instructionsAllocated = sizeof(table.instructions) / sizeof(table.instructions[0]);
  for (int i=0; i<instructionsAllocated; i++) {
    table.instructions[i].name = NULL;
    table.instructions[i].print = NULL;
    table.instructions[i].tryEval = NULL;
  }

  // init with known instructions
  Inst instructions[]      = {
      [I_LOAD_CONST]       = { .name = "I_LOAD_CONST",      .print = printInstAndIndex,   .tryEval = tryLoadConstEval },
      [I_LOAD_LOCAL]       = { .name = "I_LOAD_LOCAL",      .print = printInstAndIndex,   .tryEval = tryLoadLocalEval },
      [I_STORE_LOCAL]      = { .name = "I_STORE_LOCAL",     .print = printInstAndIndex,   .tryEval = tryStoreLocalEval },
      [I_INVOKE_DYN]       = { .name = "I_INVOKE_DYN",      .print = printInst,           .tryEval = tryInvokeDynEval },
      [I_INVOKE_DYN_TAIL]  = { .name = "I_INVOKE_DYN_TAIL", .print = printInst,           .tryEval = tryInvokeDynTailEval },
      [I_RET]              = { .name = "I_RET",             .print = printInst,           .tryEval = tryRetEval },
      [I_CMP]              = { .name = "I_CMP",             .print = printInst,           .tryEval = tryCmpEval },
      [I_JMP]              = { .name = "I_JMP",             .print = printInstAndIndex,   .tryEval = tryJmpEval },
      [I_JMP_IF]           = { .name = "I_JMP_IF",          .print = printInstAndIndex,   .tryEval = tryJmpIfEval },
      [I_JMP_IF_NOT]       = { .name = "I_JMP_IF_NOT",      .print = printInstAndIndex,   .tryEval = tryJmpIfNotEval },
      [I_ADD]              = { .name = "I_ADD",             .print = printInst,           .tryEval = tryAddEval },
      [I_SUB]              = { .name = "I_SUB",             .print = printInst,           .tryEval = trySubEval },
      [I_DEF_VAR]          = { .name = "I_DEF_VAR",         .print = printInstAndIndex,   .tryEval = tryDefVarEval },
      [I_LOAD_VAR]         = { .name = "I_LOAD_VAR",        .print = printInstAndIndex,   .tryEval = tryLoadVarEval },
      [I_LOAD_CLOSURE]     = { .name = "I_LOAD_CLOSURE",    .print = printInstAndIndex,   .tryEval = tryLoadClosureEval },
      [I_SWAP]             = { .name = "I_SWAP",            .print = printInst,           .tryEval = trySwapEval },
      [I_SET_HANDLER]      = { .name = "I_SET_HANDLER",     .print = printInstAndIndex2x, .tryEval = trySetHandlerEval },
      [I_CLEAR_HANDLER]    = { .name = "I_CLEAR_HANDLER",   .print = printInst,           .tryEval = tryClearHandlerEval },

      [I_CONS]             = { .name = "I_CONS",            .print = printInst,           .tryEval = tryConsEval },
      [I_FIRST]            = { .name = "I_FIRST",           .print = printInst,           .tryEval = tryFirstEval},
      [I_REST]             = { .name = "I_REST",            .print = printInst,           .tryEval = tryRestEval },
      [I_SET_MACRO]        = { .name = "I_SET_MACRO",       .print = printInst,           .tryEval = trySetMacroEval},
      [I_GET_MACRO]        = { .name = "I_GET_MACRO",       .print = printInst,           .tryEval = tryGetMacroEval},
      [I_GC]               = { .name = "I_GC",              .print = printInst,           .tryEval = tryGCEval},
      [I_GET_TYPE]         = { .name = "I_GET_TYPE",        .print = printInst,           .tryEval = tryGetTypeEval },
      [I_PRN]              = { .name = "I_PRN",             .print = printInst,           .tryEval = tryPrnEval },


//      [I_NEW]         = { .name = "I_NEW",         .print = printUnknown},
//      [I_GET_FIELD]   = { .name = "I_GET_FIELD",   .print = printUnknown},
//      [I_SET_FIELD]   = { .name = "I_SET_FIELD",   .print = printUnknown},
//      [I_LOAD_ARRAY]  = { .name = "I_LOAD_ARRAY",  .print = printUnknown},
//      [I_STORE_ARRAY] = { .name = "I_STORE_ARRAY", .print = printUnknown},
// requires garbage collection
//      case I_NEW:           // (8), objlen (16) | (-> objectref)
//      case I_GET_FIELD:     // (8), typeIndex  (16) | (objectref -> value)
//      case I_SET_FIELD:     // (8), typeIndex  (16) | (objectref, value ->)
//      case I_NEW_ARRAY:     // (8), objlen (16) | (arraylen -> objectref)
//      case I_LOAD_ARRAY:    // (8)              | (objectref, typeIndex -> value)
//      case I_STORE_ARRAY:   // (8)              | (objectref, typeIndex, value ->)
  };
  memcpy(table.instructions, instructions, sizeof(instructions));
  table.numInstructions = sizeof(instructions) / sizeof(instructions[0]);

  return table;
}

/*
 * code printing utils based on InstTable metadata
 */

const char* getInstName(InstTable *instTable, uint8_t inst) {
  return instTable->instructions[inst].name;
}

void _printCodeArray(InstTable *table, uint8_t *code, uint16_t codeLength) {
  for (int i=0; i<codeLength; i++) {
    Inst inst = table->instructions[code[i]];
    inst.print(&i, inst.name, code);
  }
}

void _printFnConstant(InstTable *table, FnConstant fnConst) {

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

void _printCodeUnit(InstTable *table, CodeUnit *unit) {

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

/*
 * value type definitions
 */

bool isTruthyYes(Value v) { return true; }
bool isTruthyNo(Value v) { return false; }
bool isTruthyBool(Value v) { return v.value; }

RetVal tryRelocateChildrenFn(VM_t vm, void *oldHeap, void *obj, Error *error) {
  RetVal ret;

  Fn *fn = obj;
  for (uint16_t i=0; i<fn->numConstants; i++) {
    throws(relocate(vm, oldHeap, &fnConstants(fn)[i], error));
  }

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryRelocateChildrenList(VM_t vm, void *oldHeap, void *obj, Error *error) {
  RetVal ret;

  Cons *cons = obj;
  throws(relocate(vm, oldHeap, &cons->value, error));
  throws(relocate(vm, oldHeap, &cons->next, error));

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryRelocateChildrenClosure(VM_t vm, void *oldHeap, void *obj, Error *error) {
  RetVal ret;

  Closure *closure = obj;
  throws(relocate(vm, oldHeap, &closure->fn, error));
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    throws(relocate(vm, oldHeap, &closureCaptures(closure)[i], error));
  }

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryPrnNil(VM_t vm, Value result, Expr *expr, Error *error) {
  expr->type = N_NIL;
  return R_SUCCESS;
}

RetVal tryPrnInt(VM_t vm, Value result, Expr *expr, Error *error) {
  expr->type = N_NUMBER;
  expr->number.value = result.value;
  return R_SUCCESS;
}

RetVal tryPrnBool(VM_t vm, Value result, Expr *expr, Error *error) {
  expr->type = N_BOOLEAN;
  expr->boolean.value = result.value;
  return R_SUCCESS;
}

RetVal tryPrnFn(VM_t vm, Value result, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<function>";
  expr->string.length = wcslen(function);
  return tryCopyText(function, &expr->string.value, expr->string.length, error);
}

RetVal tryPrnCFn(VM_t vm, Value result, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<c-function>";
  expr->string.length = wcslen(function);
  return tryCopyText(function, &expr->string.value, expr->string.length, error);
}

RetVal tryPrnClosure(VM_t vm, Value result, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<closure>";
  expr->string.length = wcslen(function);
  return tryCopyText(function, &expr->string.value, expr->string.length, error);
}

RetVal equalsString(VM *vm, Value value, wchar_t *cmpStr, bool *equals, Error *error) {
  RetVal ret;

  *equals = false;
  if (value.type == VT_STR) {

    String *str = NULL;
    throws(deref(&vm->gc, (void*)&str, value.value, error));

    if (wcscmp(stringValue(str), cmpStr) == 0) {
      *equals = true;
    }
  }

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryPrnStr(VM_t vm, Value result, Expr *expr, Error *error) {
  RetVal ret;

  String *str = NULL;
  throws(deref(&vm->gc, (void*)&str, result.value, error));

  expr->type = N_STRING;
  expr->string.length = str->length;
  throws(tryCopyText(stringValue(str), &expr->string.value, expr->string.length, error));

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryPrnSymbol(VM_t vm, Value result, Expr *expr, Error *error) {
  RetVal ret;

  Symbol *sym = NULL;
  throws(deref(&vm->gc, (void*)&sym, result.value, error));

  expr->type = N_SYMBOL;
  expr->symbol.length = sym->length;
  throws(tryCopyText(symbolValue(sym), &expr->symbol.value, expr->string.length, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrnKeyword(VM_t vm, Value result, Expr *expr, Error *error) {
  RetVal ret;

  Keyword *kw = NULL;
  throws(deref(&vm->gc, (void*)&kw, result.value, error));

  expr->type = N_KEYWORD;
  expr->keyword.length = kw->length;
  throws(tryCopyText(keywordValue(kw), &expr->keyword.value, expr->string.length, error));

  return R_SUCCESS;
  failure:
  return ret;
}

bool isEmpty(Value value) {
  return value.type == VT_NIL;
}

typedef struct Property {
  Keyword *key;
  Value value;
} Property;

RetVal tryReadProperty(VM *vm, Value *ptr, Property *p, Error *error) {
  RetVal ret;

  if (ptr->type != VT_LIST) {
    throwRuntimeError(error, "expected property list: %s",
                      getValueTypeName(vm, ptr->type));
  }

  Cons *properties;
  throws(deref(&vm->gc, (void*)&properties, ptr->value, error));

  if (properties->value.type != VT_KEYWORD) {
    throwRuntimeError(error, "expected keyword for property key: %s",
                      getValueTypeName(vm, properties->value.type));
  }

  throws(deref(&vm->gc, (void*)&p->key, properties->value.value, error));

  if (isEmpty(properties->next)) {
    throwRuntimeError(error, "expected value for property but only found a key: %ls", keywordValue(p->key));
  }

  throws(deref(&vm->gc, (void*)&properties, properties->next.value, error));
  p->value = properties->value;

  *ptr = properties->next;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryPrnList(VM_t vm, Value result, Expr *expr, Error *error) {
  RetVal ret;

  Cons *cons;
  throws(deref(&vm->gc, (void*)&cons, result.value, error));

  expr->type = N_LIST;

  Value metadata = cons->header.metadata;
  while (!isEmpty(metadata)) {

    Property p;
    throws(tryReadProperty(vm, &metadata, &p, error));

    if (wcscmp(L"line-number", keywordValue(p.key)) == 0) {

      if (p.value.type != VT_UINT) {
        throwRuntimeError(error, "expected line-number property value to be an int: %s",
                          getValueTypeName(vm, p.value.type));
      }

      expr->source.isSet = true;
      expr->source.lineNumber = p.value.value;
    }
    else {
      // ignore property
    }
  }

  listInitContents(&expr->list);
  Expr *elem;

  tryMalloc(elem, sizeof(Expr), "Expr");
  exprInitContents(elem);

  throws(tryVMPrn(vm, cons->value, elem, error));
  throws(tryListAppend(&expr->list, elem, error));

  while (cons->next.type != VT_NIL) {

    if (cons->next.type != VT_LIST) {
      throwRuntimeError(error, "this should always be a type of VT_LIST: %s",
                        getValueTypeName(vm, cons->next.type));
    }

    throws(deref(&vm->gc, (void *) &cons, cons->next.value, error));

    tryMalloc(elem, sizeof(Expr), "Expr");
    exprInitContents(elem);

    throws(tryVMPrn(vm, cons->value, elem, error));
    throws(tryListAppend(&expr->list, elem, error));
  }

  return R_SUCCESS;
  failure:
    return ret;
}

ValueTypeTable valueTypeTableCreate() {
  ValueTypeTable table;

  // init table with blanks
  uint16_t valueTypesAllocated = sizeof(table.valueTypes) / sizeof(table.valueTypes[0]);
  for (int i=0; i<valueTypesAllocated; i++) {
    table.valueTypes[i].name = NULL;
    table.valueTypes[i].isHeapObject = false;
    table.valueTypes[i].isTruthy = NULL;
    table.valueTypes[i].tryRelocateChildren = NULL;
    table.valueTypes[i].tryPrn = NULL;
  }

  // init with known value types
  ValueTypeInfo valueTypes [] = {
      [VT_NIL]       = {.name = "nil",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyNo,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnNil},
      [VT_UINT]      = {.name = "uint",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnInt},
      [VT_BOOL]      = {.name = "bool",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyBool,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnBool},
      [VT_FN]        = {.name = "fn",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = &tryRelocateChildrenFn,
                        .tryPrn = &tryPrnFn},
      [VT_STR]       = {.name = "str",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnStr},
      [VT_SYMBOL]    = {.name = "symbol",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnSymbol},
      [VT_KEYWORD]   = {.name = "keyword",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnKeyword},
      [VT_LIST]      = {.name = "list",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = &tryRelocateChildrenList,
                        .tryPrn = &tryPrnList},
      [VT_CLOSURE]   = {.name = "closure",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = &tryRelocateChildrenClosure,
                        .tryPrn = &tryPrnClosure},
      [VT_CFN]       = {.name = "cfn",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .tryRelocateChildren = NULL,
                        .tryPrn = &tryPrnCFn},
  };
  memcpy(table.valueTypes, valueTypes, sizeof(valueTypes));
  table.numValueTypes = sizeof(valueTypes) / sizeof(valueTypes[0]);

  return table;
}

void printEvalError(ExecFrame_t frame, Error *error) {

  printf("unhandled error: %ls", error->message);

  ExecFrame_t current = frame;
  while (true) {

    wchar_t *fnName;
    if (hasFnName(current)) {
      Text text;
      textInitContents(&text);
      if (getFnName(current, &text, error) != R_SUCCESS) {
        break;
      }
      fnName = text.value;
    }
    else {
      fnName = L"<root>";
    }

    wchar_t *file = L"core.lsp";
    uint16_t lineNo = 100;
    printf("\t%ls(%ls:%u)\n", fnName, file, lineNo);

    if (!hasParent(current)) {
      break;
    }
    else {
      if (getParent(current, &current, error) != R_SUCCESS) {
        break;
      }
    }
  }
}

RetVal tryExceptionMake(ExecFrame_t frame, VMException *exception, Error *error) {
  RetVal ret;

  Error reference = *error;

  wchar_t msg[ERROR_MSG_LENGTH];
  exceptionInitContents(exception);

  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", reference.message);
  throws(tryTextMake(msg, &exception->message, wcslen(msg), error));

  uint64_t numFrames = 0;
  {
    ExecFrame_t current = frame;
    while (true) {
      numFrames++;
      if (!hasParent(current)) {
        break;
      }
      else {
        throws(getParent(current, &current, error) != R_SUCCESS);
      }
    }
  }

  // native frame
  numFrames++;

  exception->frames.length = numFrames;
  tryMalloc(exception->frames.elements, sizeof(VMExceptionFrame) * numFrames, "VMExceptionFrame array");

  { // native frame

    VMExceptionFrame *f = &exception->frames.elements[0];
    exFrameInitContents(f);

    f->functionName.length = strlen(reference.functionName) + 1;
    tryMalloc(f->functionName.value, f->functionName.length * sizeof(wchar_t), "wide string");
    swprintf(f->functionName.value, f->functionName.length, L"%s", reference.functionName);

    f->unknownSource = false;

    char* fileName = basename((char *) reference.fileName);
    f->fileName.length = strlen(fileName) + 1;
    tryMalloc(f->fileName.value, f->fileName.length * sizeof(wchar_t), "wide string");
    swprintf(f->fileName.value, f->fileName.length, L"%s", fileName);

    f->lineNumber = reference.lineNumber;
  }

  ExecFrame_t current = frame;
  for (uint64_t i=1; i<numFrames; i++) {

    VMExceptionFrame *f = &exception->frames.elements[i];
    exFrameInitContents(f);

    if (hasFnName(current)) {
      Text text;
      throws(getFnName(current, &text, error));
      throws(tryTextCopy(&text, &f->functionName, error));
    }
    else {
      wchar_t *name = L"<root>\0";
      throws(tryTextMake(name, &f->functionName, wcslen(name), error));
    }

    if (hasSourceTable(current)) {
      f->unknownSource = false;
      getFileName(current, &f->fileName);
      getLineNumber(current, &f->lineNumber);
    }

    if (hasParent(current)) {
      throws(getParent(current, &current, error));
    }
  }

  return R_SUCCESS;

  failure:
    exceptionFreeContents(exception);
    return ret;
}

RetVal tryExceptionPrint(VMException *e, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(&b, error));

  throws(tryStringBufferAppendStr(b, error->message, error));

  wchar_t msg[ERROR_MSG_LENGTH];

  for (uint64_t i=0; i<e->frames.length; i++) {
    VMExceptionFrame *f = &e->frames.elements[i];

    if (f->unknownSource) {
      swprintf(msg, ERROR_MSG_LENGTH, L"\tat %ls(Unknown Source)\n", f->functionName.value);
    }
    else {
      swprintf(msg, ERROR_MSG_LENGTH, L"\tat %ls(%ls:%" PRIu64 ")\n", f->functionName.value, f->fileName.value,
          f->lineNumber);
    }
    throws(tryStringBufferAppendStr(b, msg, error));
  }

  wchar_t *output;
  throws(tryCopyText(stringBufferText(b), &output, stringBufferLength(b), error));
  stringBufferFree(b);

  *ptr = output;
  return R_SUCCESS;

  failure:
  stringBufferFree(b);
  return ret;
}

RetVal tryExceptionPrintf(VMException *e, Error *error) {
  RetVal ret;

  wchar_t *msg;
  throws(tryExceptionPrint(e, &msg, error));
  printf("%ls\n", msg);
  free(msg);

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryFrameEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint8_t inst;
  TryEval tryEval;

  while (true) {

    if (hasResult(frame)) {
      if (!hasParent(frame)) {
        break;
      }
      else {
        Value result;
        ExecFrame_t parent;

        throws(getResult(frame, &result, error));
        throws(getParent(frame, &parent, error));
        throws(pushOperand(parent, result, error));

        popFrame(frame);
      }
    }

    throws(readInstruction(frame, &inst, error));
    tryEval = vm->instTable.instructions[inst].tryEval;

    if (tryEval == NULL) {
      throwRuntimeError(error, "instruction unimplemented: %s (%u)", getInstName(&vm->instTable, inst), inst);
    }

    ret = tryEval(vm, frame, error);

    if (ret != R_SUCCESS) {
      VMException ex;
      throws(tryExceptionMake(frame, &ex, error));
      setException(frame, ex);
      break;
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

  // create an exception
  // - allocate it
  // - set its payload with the error message
  // - initialize its stack trace information
  // - the entire exception is just lists of lists (payload, trace)
  // walk up the stack until we find an error handler
  // if no error handler is found, print the stack trace and goto failure
  // if an error handler is found
  // - pop the stack until the frame with the error handler is the current frame
  // - set the exception as the local index for the handler's exception binding
  // - jump to the error handler's jump address

  /*
   * TODO: how exception handling should work, revised
   * - analyzer defines a try/catch form, where the catch introduces a binding in the binding table + some forms to eval
   * - compiler computes the jumpAddress for the catch block
   * - compiler emits I_SET_HANDLER with 2 index parameters (jumpAddress and exceptionLocalIndex)
   * - vm makes use of error handlers *here* as needed
   *
   * this means no need for lambda/closure execution at error handling time
   */

/*
 * The OpStack
 */

void opStackInitContents(OpStack *stack) {
  stack->usedDepth = 0;
  stack->maxDepth = 0;
  stack->stack = NULL;
}

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
    throwRuntimeError(error, "cannot allocate op stack greater than max %" PRIu64, stack->maxDepth);
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

/*
 * ExecFrame Implementation
 */

typedef struct ExecFrame ExecFrame;

typedef struct ExecFrame {
  ExecFrame *parent;

  Value fnRef;
  Fn *fn; // TODO: the collector will have to treat relocating functions specially to make this reference work

  Value *locals;
  OpStack *opStack;
  Value result;
  bool resultAvailable;
  uint16_t pc;

  ExceptionHandler handler;
  bool handlerSet;

  VMException exception;
  bool exceptionSet;
} ExecFrame;

void handlerInitContents(ExceptionHandler *h) {
  h->localIndex = 0;
  h->jumpAddress = 0;
}

void frameInitContents(ExecFrame *frame) {
  frame->parent = NULL;
  frame->fnRef = nil();
  frame->fn = NULL;
  frame->locals = NULL;
  frame->opStack = NULL;
  frame->resultAvailable = 0;
  frame->result.type = VT_NIL;
  frame->result.value = 0;
  frame->pc = 0;
  handlerInitContents(&frame->handler);
  frame->handlerSet = false;
  exceptionInitContents(&frame->exception);
  frame->exceptionSet = false;
}

RetVal readInstruction(ExecFrame *frame, uint8_t *ptr, Error *error) {
  RetVal ret;

  if (frame->pc >= frame->fn->codeLength) {
    throwRuntimeError(error, "cannot read next instruction, no instructions left");
  }

  *ptr = fnCode(frame->fn)[frame->pc];
  frame->pc += 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal readIndex(ExecFrame *frame, uint16_t *ptr, Error *error) {
  RetVal ret;

  if (frame->pc + 1 >= frame->fn->codeLength) {
    throwRuntimeError(error, "cannot read next instruction, no instructions left");
  }

  uint8_t *code = fnCode(frame->fn);
  uint16_t pc = frame->pc;
  *ptr = (code[pc] << 8) | code[pc + 1];
  frame->pc += 2;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal setPc(ExecFrame *frame, uint16_t newPc, Error *error) {
  RetVal ret;

  if (newPc >= frame->fn->codeLength) {
    throwRuntimeError(error, "no such instruction: %u", newPc);
  }

  frame->pc = newPc;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getConst(ExecFrame *frame, uint16_t constantIndex, Value *ptr, Error *error) {
  RetVal ret;

  if (constantIndex >= frame->fn->numConstants) {
    throwRuntimeError(error, "no such constant: %u", constantIndex);
  }

  *ptr = fnConstants(frame->fn)[constantIndex];
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getLocal(ExecFrame *frame, uint16_t localIndex, Value *ptr, Error *error) {
  RetVal ret;

  if (localIndex >= frame->fn->numLocals) {
    throwRuntimeError(error, "no such local: %u", localIndex);
  }

  *ptr = frame->locals[localIndex];
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getLocalRef(ExecFrame *frame, uint16_t localIndex, Value **ptr, Error *error) {
  RetVal ret;

  if (localIndex >= frame->fn->numLocals) {
    throwRuntimeError(error, "no such local: %u", localIndex);
  }

  *ptr = &frame->locals[localIndex];
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal setLocal(ExecFrame *frame, uint16_t localIndex, Value value, Error *error) {
  RetVal ret;

  if (localIndex >= frame->fn->numLocals) {
    throwRuntimeError(error, "no such local: %u", localIndex);
  }

  frame->locals[localIndex] = value;
  return R_SUCCESS;

  failure:
  return ret;
}

uint16_t numLocals(ExecFrame *frame) {
  return frame->fn->numLocals;
}

uint64_t numOperands(ExecFrame *frame) {
  return frame->opStack->usedDepth;
}

RetVal getOperandRef(ExecFrame *frame, uint64_t opIndex, Value **ptr, Error *error) {
  RetVal ret;

  if (opIndex >= frame->opStack->usedDepth) {
    throwRuntimeError(error, "no such operand: %" PRIu64, opIndex);
  }

  *ptr = &frame->opStack->stack[opIndex];
  return R_SUCCESS;

  failure:
  return ret;
}

uint16_t pushOperand(ExecFrame *frame, Value value, Error *error) {
  RetVal ret;

  throws(tryOpStackPush(frame->opStack, value, error));
  return R_SUCCESS;

  failure:
  return ret;
}

uint16_t popOperand(ExecFrame *frame, Value *value, Error *error) {
  RetVal ret;

  throws(tryOpStackPop(frame->opStack, value, error));
  return R_SUCCESS;

  failure:
  return ret;
}

Value getFnRef(ExecFrame *frame) {
  return frame->fnRef;
}

RetVal setFnRef(VM *vm, ExecFrame *frame, Value value, Error *error) {
  RetVal ret;

  frame->fnRef = value;
  frame->fn = NULL;

  throws(deref(&vm->gc, (void*)&frame->fn, value.value, error));

  return R_SUCCESS;
  failure:
    return ret;

}

bool hasResult(ExecFrame *frame) {
  return frame->resultAvailable;
}

bool hasParent(ExecFrame *frame) {
  return frame->parent != NULL;
}

RetVal getParent(ExecFrame *frame, ExecFrame **ptr, Error *error) {
  RetVal ret;

  if (frame->parent == NULL) {
    throwRuntimeError(error, "no parent available");
  }

  *ptr = frame->parent;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal setResult(ExecFrame *frame, Value result, Error *error) {
  RetVal ret;

  if (frame->resultAvailable) {
    throwRuntimeError(error, "result already set");
  }

  frame->result = result;
  frame->resultAvailable = true;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getResult(ExecFrame *frame, Value *ptr, Error *error) {
  RetVal ret;

  if (!frame->resultAvailable) {
    throwRuntimeError(error, "result not set");
  }

  *ptr = frame->result;
  return R_SUCCESS;

  failure:
  return ret;
}

bool hasHandler(ExecFrame *frame) {
  return frame->handlerSet;
}

RetVal getHandler(ExecFrame_t frame, ExceptionHandler *ptr, Error *error) {
  RetVal ret;

  if (!frame->handlerSet) {
    throwRuntimeError(error, "handler not set");
  }

  *ptr = frame->handler;
  return R_SUCCESS;

  failure:
    return ret;
}

void setHandler(ExecFrame_t frame, ExceptionHandler handler) {
  frame->handler = handler;
  frame->handlerSet = true;
}

void clearHandler(ExecFrame_t frame) {
  handlerInitContents(&frame->handler);
  frame->handlerSet = false;
}

bool hasFnName(ExecFrame *frame) {
  return frame->fn->hasName;
}

RetVal getFnName(ExecFrame_t frame, Text *name, Error *error) {
  RetVal ret;

  if (!frame->fn->hasName) {
    throwRuntimeError(error, "no fn name found");
  }

  name->value = fnName(frame->fn);
  name->length = frame->fn->nameLength;
  return R_SUCCESS;

  failure:
    return ret;
}

bool hasSourceTable(ExecFrame *frame) {
  return frame->fn->hasSourceTable;
}

bool getLineNumber(ExecFrame *frame, uint64_t *lineNumber) {
  if (frame->fn->hasSourceTable) {
    for (uint64_t i=0; i<frame->fn->numLineNumbers; i++) {
      LineNumber *l = &fnLineNumbers(frame->fn)[i];
      if (l->startInstructionIndex >= frame->pc) {
        break;
      }
      else {
        *lineNumber = l->lineNumber;
      }
    }
  }
  return false;
}

bool getFileName(ExecFrame_t frame, Text *fileName) {
  if (frame->fn->hasSourceTable) {
    fileName->value = fnSourceFileName(frame->fn);
    fileName->length = frame->fn->sourceFileNameLength;
    return true;
  }
  return false;
}

bool hasException(ExecFrame_t frame) {
  return frame->exceptionSet;
}

void setException(ExecFrame_t frame, VMException e) {
  frame->exception = e;
  frame->exceptionSet = true;
}

RetVal getException(ExecFrame_t frame, VMException *e, Error *error) {
  RetVal ret;

  if (!frame->exceptionSet) {
    throwRuntimeError(error, "handler not set");
  }

  *e = frame->exception;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal pushFrame(VM *vm, ExecFrame **framePtr, Value newFn, Error *error) {
  RetVal ret;

  Fn *fn = NULL;
  throws(deref(&vm->gc, (void*)&fn, newFn.value, error));

  // clean up on fail
  ExecFrame_t parent = NULL;

  ExecFrame *frame = *framePtr;
  if (frame == NULL) {
    tryMalloc(frame, sizeof(ExecFrame), "ExecFrame");
  }
  else {
    tryMalloc(parent, sizeof(ExecFrame), "ExecFrame");
    memcpy(parent, frame, sizeof(ExecFrame));
  }

  ExecFrame child;
  frameInitContents(&child);

  child.parent = parent;
  child.fnRef = newFn;
  child.fn = fn;

  tryMalloc(child.opStack, sizeof(OpStack), "OpStack");
  tryMalloc(child.locals, sizeof(Value) * child.fn->numLocals, "Value array");
  throws(tryOpStackInitContents(child.opStack, child.fn->maxOperandStackSize, error));

  *frame = child;
  *framePtr = frame;
  return R_SUCCESS;

  failure:
    if (parent != NULL) {
      free(parent);
    }
    free(child.locals);
    opStackFreeContents(child.opStack);
    return ret;
}

RetVal replaceFrame(VM *vm, ExecFrame *frame, Value newFn, Error *error) {
  RetVal ret;

  Fn *fn = NULL;
  throws(deref(&vm->gc, (void*)&fn, newFn.value, error));

  // resize locals if needed
  uint16_t newNumLocals = fn->numLocals + fn->numCaptures;
  if (newNumLocals > frame->fn->numLocals) {
    Value *resizedLocals = realloc(frame->locals, newNumLocals * sizeof(Value));
    if (resizedLocals == NULL) {
      throwMemoryError(error, "realloc Value array");
    }
    frame->fn->numLocals = newNumLocals;
    frame->locals = resizedLocals;
  }

  if (fn->maxOperandStackSize > frame->opStack->maxDepth) {
    Value *resizedStack = realloc(frame->opStack->stack, fn->maxOperandStackSize * sizeof(Value));
    if (resizedStack == NULL) {
      throwMemoryError(error, "realloc Value array");
    }
    frame->opStack->maxDepth = fn->maxOperandStackSize;
    frame->opStack->stack = resizedStack;
  }

  frame->fnRef = newFn;
  frame->fn = fn;
  frame->result = nil();
  frame->resultAvailable = false;
  frame->pc = 0;

  return R_SUCCESS;

  failure:
    return ret;
}

void popFrame(ExecFrame *frame) {

  ExecFrame *child = frame;
  ExecFrame *parent = frame->parent;

  opStackFreeContents(child->opStack);
  free(child->locals);

  if (parent != NULL) {
    *frame = *parent;
    free(parent);
  }
}

/*
 * The top level frame could just be a regular frame
 * - that has an extra local, which points to a function
 * - which happens to be the function created from the root expression and allocated on the heap
 *
 * This would remove all special cases in stack stuff, except for how that initial function gets hydrated, allocated, and bound
 *
 */

RetVal _tryVMEval(VM *vm, CodeUnit *codeUnit, Value *result, VMException *exception, bool *exceptionThrown,  Error *error) {

  RetVal ret;

  bool pushed = false;

  FnConstant c;
  constantFnInitContents(&c);
  c.numConstants = codeUnit->numConstants;
  c.constants = codeUnit->constants;
  c.code = codeUnit->code;

  Value fnRef = nil();
  throws(tryFnHydrate(vm, &c, &fnRef, error));

  ExecFrame_t frame = NULL;
  throws(pushFrame(vm, &frame, fnRef, error));
  pushed = true;

  throws(tryFrameEval(vm, frame, error));

  if (frame->exceptionSet) {
    *exceptionThrown = true;
    *exception = frame->exception;
  }
  else {
    *result = frame->result;
  }

  popFrame(frame);

  return R_SUCCESS;

  failure:
    if (pushed) {
      popFrame(frame);
    }
    return ret;
}

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, VMEvalResult *result, Error *error) {

  RetVal ret;

  Value value;
  VMException exception;
  bool exceptionThrown = false;

  throws(_tryVMEval(vm, codeUnit, &value, &exception, &exceptionThrown, error));

  if (exceptionThrown) {
    result->type = RT_EXCEPTION;
    result->exception = exception;
  }
  else {
    result->type = RT_RESULT;
    throws(tryVMPrn(vm, value, &result->result, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

/*
 * builtin procedures
 */

#define ASSERT_SEQ(value, ...) {\
  if (value.type != VT_LIST && value.type != VT_NIL) { \
    throwRuntimeError(error, "expected a list type: %s", getValueTypeName(vm, value.type)); \
  } \
}

#define ASSERT_STR(value, ...) {\
  if (value.type != VT_STR) { \
    throwRuntimeError(error, "expected a string type: %s", getValueTypeName(vm, value.type)); \
  } \
}

RetVal tryStringMakeBlank(VM *vm, ExecFrame_t frame, uint64_t length, Value *value, Error *error) {
  RetVal ret;

  String *str = NULL;

  size_t textSize = (length + 1) * sizeof(wchar_t);
  size_t strSize = sizeof(String) + textSize;

  uint64_t offset = 0;

  throws(alloc(vm, frame, strSize, (void*)&str, &offset, error));

  value->type = VT_STR;
  value->value = offset;

  stringInitContents(str);
  str->header.type = VT_STR;
  str->header.size = strSize;
  str->length = length;

  str->valueOffset = sizeof(String);
  stringValue(str)[length] = L'\0';

  return R_SUCCESS;

  failure:
  return ret;
}

/*
 * joins a sequence of strings together into one big string
 *
 * pop the args list off the stack
 * iterate over the list and compute the total size of all the strings in the list
 * push the args list back on the stack so gc can find it
 * allocate a new string that is the total size
 * pop the args list back off the stack, deref and copy each one into the new list
 * push the new string onto the stack
 */
RetVal tryStrJoinBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value strings;
  throws(popOperand(frame, &strings, error));
  ASSERT_SEQ(strings);

  uint64_t totalLength = 0;

  Value cursor = strings;
  while (cursor.type != VT_NIL) {

    Cons *seq = NULL;
    throws(deref(&vm->gc, (void*)&seq, cursor.value, error));

    ASSERT_STR(seq->value);

    String *string = NULL;
    throws(deref(&vm->gc, (void*)&string, seq->value.value, error));
    totalLength += string->length;

    cursor = seq->next;
  }

  // store the list on the op stack while we allocate since gc may happen
  throws(pushOperand(frame, strings, error));

  Value resultRef = nil();
  throws(tryStringMakeBlank(vm, frame, totalLength, &resultRef, error));
  String *result = NULL;
  throws(deref(&vm->gc, (void*)&result, resultRef.value, error));

  // get the list back again after allocation
  throws(popOperand(frame, &strings, error));

  uint64_t totalSizeWritten = 0;

  cursor = strings;
  while (cursor.type != VT_NIL) {
    Cons *seq = NULL;
    throws(deref(&vm->gc, (void*)&seq, cursor.value, error));

    String *string = NULL;
    throws(deref(&vm->gc, (void*)&string, seq->value.value, error));

    wchar_t *writePtr = (void*)result + result->valueOffset + totalSizeWritten;
    size_t textSize = string->length * sizeof(wchar_t);
    memcpy(writePtr, stringValue(string), textSize);
    totalSizeWritten += textSize;

    cursor = seq->next;
  }

  throws(pushOperand(frame, resultRef, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrStrBuiltinConf(VM *vm, ExecFrame_t frame, bool readable, Error *error) {
  RetVal ret;

  Value value;
  throws(popOperand(frame, &value, error));

  // note: using off-heap memory to construct the string
  Expr expr;
  exprInitContents(&expr);
  StringBuffer_t b = NULL;

  throws(tryVMPrn(vm, value, &expr, error));
  throws(tryStringBufferMake(&b, error));
  throws(tryExprPrnBufConf(&expr, b, readable, error));

  Value resultRef = nil();
  throws(tryStringMakeBlank(vm, frame, stringBufferLength(b), &resultRef, error));
  String *result = NULL;
  throws(deref(&vm->gc, (void*)&result, resultRef.value, error));

  memcpy(stringValue(result), stringBufferText(b), stringBufferLength(b) * sizeof(wchar_t));

  throws(pushOperand(frame, resultRef, error));

  ret = R_SUCCESS;
  goto done;

  failure:
  goto done;

  done:
    // clean up off-heap memory
    exprFreeContents(&expr);
    stringBufferFree(b);
    return ret;
}

RetVal tryPrStrBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  return tryPrStrBuiltinConf(vm, frame, true, error);
}

RetVal tryPrintStrBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  return tryPrStrBuiltinConf(vm, frame, false, error);
}

void cFnInitContents(CFn *fn) {
  objectHeaderInitContents(&fn->header);
  fn->nameLength = 0;
  fn->nameOffset = 0;
  fn->numArgs = 0;
  fn->ptr = NULL;
  fn->usesVarArgs = false;
}

RetVal tryMakeCFn(VM *vm, const wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr, Value *value, Error *error) {
  RetVal ret;

  CFn *fn = NULL;

  size_t nameLength = wcslen(name);
  size_t nameSize = (nameLength + 1) * sizeof(wchar_t);
  size_t fnSize = sizeof(CFn) + nameSize;

  uint64_t offset = 0;

  if (_alloc(&vm->gc, fnSize, (void*)&fn, &offset) == R_OOM) {
    throwRuntimeError(error, "out of memory, failed to allocate CFn: %ls", name);
  }

  value->type = VT_CFN;
  value->value = offset;

  cFnInitContents(fn);
  fn->header.type = VT_CFN;
  fn->header.size = fnSize;
  fn->nameLength = nameLength;
  fn->numArgs = numArgs;
  fn->ptr = ptr;
  fn->usesVarArgs = varArgs;

  fn->nameOffset = sizeof(CFn);
  memcpy(cFnName(fn), name, nameLength * sizeof(wchar_t));
  cFnName(fn)[nameLength] = L'\0';

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryDefineCFn(VM *vm, wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr, Error *error) {
  RetVal ret;

  size_t nameLength = wcslen(name);
  Value value = nil();

  throws(tryMakeCFn(vm, name, numArgs, varArgs, ptr, &value, error));
  throws(tryDefVar(&vm->namespaces, name, nameLength, value, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryInitCFns(VM *vm, Error *error) {
  RetVal ret;

  throws(tryDefineCFn(vm, L"cons",      2, false, tryConsEval,         error));
  throws(tryDefineCFn(vm, L"first",     1, false, tryFirstEval,        error));
  throws(tryDefineCFn(vm, L"rest",      1, false, tryRestEval,         error));
  throws(tryDefineCFn(vm, L"set-macro", 1, false, trySetMacroEval,     error));
  throws(tryDefineCFn(vm, L"get-macro", 1, false, tryGetMacroEval,     error));
  throws(tryDefineCFn(vm, L"gc",        0, false, tryGCEval,           error));
  throws(tryDefineCFn(vm, L"get-type",  1, false, tryGetTypeEval,      error));
  throws(tryDefineCFn(vm, L"prn",       1, false, tryPrnEval,          error));
  throws(tryDefineCFn(vm, L"+",         2, false, tryAddEval,          error));
  throws(tryDefineCFn(vm, L"-",         2, false, trySubEval,          error));
  throws(tryDefineCFn(vm, L"=",         2, false, tryCmpEval,          error));
  throws(tryDefineCFn(vm, L"join",      1, false, tryStrJoinBuiltin,   error));
  throws(tryDefineCFn(vm, L"pr-str",    1, false, tryPrStrBuiltin,     error));
  throws(tryDefineCFn(vm, L"print-str", 1, false, tryPrintStrBuiltin,  error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryVMInitContents(VM *vm , Error *error) {
  RetVal ret;

  vm->instTable = instTableCreate();
  vm->valueTypeTable = valueTypeTableCreate();
  throws(tryGCInitContents(&vm->gc, 1024 * 1000, error));
  throws(tryNamespacesInitContents(&vm->namespaces, error));
  throws(tryInitCFns(vm, error));

  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFreeContents(VM *vm) {
  if (vm != NULL) {
    GCFreeContents(&vm->gc);
    namespacesFreeContents(&vm->namespaces);
  }
}

RetVal tryVMMake(VM **ptr , Error *error) {
  RetVal ret;

  tryMalloc(*ptr, sizeof(VM), "VM malloc");
  throws(tryVMInitContents(*ptr, error));

  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFree(VM *vm) {
  if (vm != NULL) {
    vmFreeContents(vm);
    free(vm);
  }
}

/*
 * External code printing utilities
 */

void printCodeArray(uint8_t *code, uint16_t codeLength) {
  InstTable table = instTableCreate();
  _printCodeArray(&table, code, codeLength);
}

void printCodeUnit(CodeUnit *unit) {
  InstTable table = instTableCreate();
  _printCodeUnit(&table, unit);
}



/*
 * THINKING ABOUT FFI
 * goal: bind values to vars, that when invoked, invoke c functions directly
 * - c functions must be enumerated and mapped from names to function pointers
 * - once a function has been resolved, it still has to be invoked:
 *
 *     Invoke an extern assembly function, which takes as arguments:
 *       - a pointer to the VM
 *       - a pointer to the C function to be called
 *       - the c function arguments as an array of 64-bit values
 *       - an Error pointer
 *
 *     The assembly function pushes the arguments onto the stack and invokes
 *
 */
