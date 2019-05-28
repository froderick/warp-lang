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

void codeInitContents(Code *code) {
  code->maxOperandStackSize = 0;
  code->numLocals = 0;
  code->hasSourceTable = false;
  code->code = NULL;
  code->codeLength = 0;
  sourceTableInitContents(&code->sourceTable);
}

void constantMetaPropertyInit(ConstantMetaProperty *p) {
  p->keyIndex = 0;
  p->valueIndex = 0;
}

void constantMetaInit(ConstantMeta *c) {
  c->numProperties = 0;
  c->properties = NULL;
}

void constantFnInitContents(FnConstant *fnConst) {
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

void codeUnitInitContents(CodeUnit *codeUnit) {
  codeUnit->constants = NULL;
  codeUnit->numConstants = 0;
  codeInitContents(&codeUnit->code);
}

void exFrameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

void framesInitContents(VMExceptionFrames *f) {
  f->length = 0;
  f->elements = NULL;
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

void evalResultInitContents(VMEvalResult *r) {
  r->type = RT_NONE;
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
   VT_ARRAY,
   VT_MAP,
   VT_MAP_ENTRY,
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
  uint32_t hash;
} String;

wchar_t* stringValue(String *x) { return (void*)x + x->valueOffset; }

typedef struct Symbol {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
  uint32_t hash;
} Symbol;

wchar_t* symbolValue(Symbol *x) { return (void*)x + x->valueOffset; }

typedef struct Keyword {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
  uint32_t hash;
} Keyword;

wchar_t* keywordValue(Keyword *x) { return (void*)x + x->valueOffset; }

typedef struct Cons {
  ObjectHeader header;

  Value value;
  Value next; // this must be a Cons, or Nil
} Cons;

typedef struct Array {
  ObjectHeader header;

  uint64_t length;
  size_t elementsOffset;
} Array;

Value* arrayElements(Array *x) { return (void*)x + x->elementsOffset; }

typedef struct MapEntry {
  ObjectHeader header;

  Value key;
  Value value;
  uint32_t hash;
} MapEntry;

typedef struct Map {
  ObjectHeader header;

  uint64_t size;
  Value array;
} Map;

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

typedef void (*RelocateChildren)(VM_t vm, void *oldHeap, void *obj);
typedef RetVal (*TryPrn)(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error);
typedef RetVal (*TryHashCode)(VM_t vm, Value value, uint32_t *hash, Error *error);
typedef RetVal (*TryEquals)(VM_t vm, Value this, Value that, bool *equal, Error *error);

typedef struct ValueTypeInfo {
  const char *name;
  bool isHeapObject;
  bool (*isTruthy)(Value value);
  RelocateChildren relocateChildren;
  TryPrn tryPrn;
  TryHashCode tryHashCode;
  TryEquals tryEquals;
} ValueTypeInfo;

typedef struct ValueTypeTable {
  uint8_t numValueTypes;
  ValueTypeInfo valueTypes[256];
} ValueTypeTable;

// vm state


/*
 * TODO: if I add a global roots pointers registry and an API to add/remove pointers to roots from this registry
 * then I can protect myself from GC even mid-instruction.
 *
 * I could also add a stack-frame dedicated space for this, so as to avoid dealing with linked lists globally
 * and to get automatic cleanup of temporary roots. This would mean that my stack frames would no longer be
 * of a fixed size, but this is already technically true because of the way I'm using the opstack for this purpose.
 *
 * Also, CFns really shouldn't be touching the stack frame directly, there should be a way to bundle up those
 * arguments properly.
 *
 * Also, Pairs (lists, now) are a good idea to build directly into the VM.
 */


/*
 * RefRegistry data structures
 */

typedef struct RefElem RefElem;

typedef struct RefElem {
  Value heapObject;
  RefElem *prev, *next;
} RefElem;

typedef struct RefRegistry {
  RefElem *used;
  RefElem *free;
} RefRegistry;

/*
 * A Ref is really a direct memory pointer to a RefElem struct.
 */
typedef uint64_t Ref;

typedef struct VM {
  GC gc;
  Namespaces namespaces;
  InstTable instTable;
  ValueTypeTable valueTypeTable;
  RefRegistry refs;
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
Value* getLocalRef(ExecFrame_t frame, uint16_t localIndex);
uint16_t numLocals(ExecFrame_t frame);
uint64_t numOperands(ExecFrame_t frame);
Value* getOperandRef(ExecFrame_t frame, uint64_t opIndex);
uint16_t pushOperand(ExecFrame_t frame, Value value, Error *error);
uint16_t popOperand(ExecFrame_t frame, Value *value, Error *error);
Value getFnRef(ExecFrame_t frame);
void setFnRef(VM *vm, ExecFrame_t frame, Value value);

bool hasResult(ExecFrame_t frame);
bool hasParent(ExecFrame_t frame);
ExecFrame_t getParent(ExecFrame_t frame);
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

void relocateChildren(VM *vm, ValueType type, void *oldHeap, void *obj) {
  RelocateChildren relocate = vm->valueTypeTable.valueTypes[type].relocateChildren;
  if (relocate != NULL) {
    relocate(vm, oldHeap, obj);
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

void GCCreate(GC *gc, uint64_t maxHeapSize) {
  GCInitContents(gc);

  gc->heapSize = maxHeapSize;
  gc->heapMemorySize = gc->heapSize * 2;

  gc->heapMemory = malloc(gc->heapMemorySize);
  if (gc->heapMemory == NULL) {
    explode("failed to allocate memory for GC");
  }

  memset(gc->heapMemory, 0, gc->heapMemorySize);

  gc->heapA = gc->heapMemory;
  gc->heapB = gc->heapA + gc->heapSize;
  gc->currentHeap = gc->heapA;
  gc->currentHeapEnd = gc->currentHeap + gc->heapSize;
  gc->allocPtr = gc->currentHeap;
}

void collect(VM *vm, ExecFrame_t frame);

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
void* alloc(VM *vm, ExecFrame_t frame, uint64_t length, Value *value) {

  void *ptr = NULL;
  uint64_t offset = 0;

  int success = _alloc(&vm->gc, length, &ptr, &offset);

  if (success == R_OOM) {
    collect(vm, frame);

    success = _alloc(&vm->gc, length, &ptr, &offset);

    if (success == R_OOM) {
      explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

  value->value = offset;

  return ptr;
}

void* deref(GC *gc, Value value) {
  uint64_t offset = value.value;
  if (value.value > gc->heapSize) {
    explode("invalid memory address");
  }
  return gc->currentHeap + offset;
}

bool inCurrentHeap(GC *gc, void *ptr) {
  return gc->currentHeap <= ptr && ptr < gc->currentHeapEnd;
}

void relocate(VM *vm, void *oldHeap, Value *value) {

  if (!isHeapObject(vm, *value)) {
    // only relocate heap objects
    return;
  }

  GC *gc = &vm->gc;

  void *ptr = NULL;
  if (value->value > gc->heapSize) {
    explode("invalid memory address");
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
      explode("out of memory, cannot allocate %lu bytes mid-gc", size);
    }

    memcpy(newPtr, ptr, size);
    value->value = newPtr - gc->currentHeap;

    *forwardPtr = newPtr;
  }
}

uint64_t now() {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC_RAW, &now);
  uint64_t millis = now.tv_nsec / 1000000;
  return millis;
}

void collect(VM *vm, ExecFrame_t frame) {

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

  // relocate refs
  RefElem *refElem = vm->refs.used;
  while (refElem != NULL) {
    relocate(vm, oldHeap, &refElem->heapObject);
    refElem = refElem->next;
  }

  // relocate var roots
  for (uint64_t i=0; i<vm->namespaces.numNamespaces; i++) {
    Namespace *ns = &vm->namespaces.namespaces[i];
    for (uint64_t j=0; j<ns->localVars.length; j++) {
      Var *var = &ns->localVars.vars[j];
      relocate(vm, oldHeap, &var->value);
    }
  }

  // relocate call stack roots
  ExecFrame_t current = frame;
  while (true) {

    // relocate fnRef
    {
      Value oldFnRef = getFnRef(current);
      Value newFnRef = oldFnRef;
      relocate(vm, oldHeap, &newFnRef);

      if (oldFnRef.value != newFnRef.value) {
        setFnRef(vm, current, newFnRef);
      }
    }

    uint16_t locals = numLocals(current);
    for (uint16_t i=0; i<locals; i++) {
      Value *val = getLocalRef(current, i);
      relocate(vm, oldHeap, val);
    }

    uint64_t operands = numOperands(current);
    for (uint64_t i=0; i<operands; i++) {
      Value *val = getOperandRef(current, i);
      relocate(vm, oldHeap, val);
    }

    if (!hasParent(current)) {
      break;
    }
    else {
      current = getParent(current);
    }
  }

  void *scanptr = vm->gc.currentHeap;

  // relocate all the objects this object references
  while (scanptr < vm->gc.allocPtr) {
    ObjectHeader *header = scanptr;
    relocateChildren(vm, header->type, oldHeap, scanptr);
    scanptr += header->size;
  }

  uint64_t newHeapUsed = vm->gc.allocPtr - vm->gc.currentHeap;
  uint64_t sizeRecovered = oldHeapUsed - newHeapUsed;
  uint64_t end = now();
  uint64_t duration = end - start;

  printf("gc: completed, %" PRIu64 " bytes recovered, %" PRIu64 " bytes used, took %" PRIu64 "ms\n", sizeRecovered, newHeapUsed, duration);
}

/* RefRegistry implementation
 *
 * The purpose of a registry is to hold references to heap objects from c
 * code in such a way that the collector can discover them and avoid premature
 * collection. Holding these references directly in c code is a problem
 * because the copying gc needs to be able to rewrite all the references
 * periodically. The c call stack is unavailable to the collector, so a layer
 * of indirection is needed to accomplish this.
 *
 * The VM has a single global registry for now, which is based on a
 * doubly-linked list. This is not good for cache locality, but we can fix this
 * later. Generally we only need refs when writing c code that manipulates
 * refs outside of places managed exclusively by the VM (stack, heap,
 * op-stack, etc).
 */

void refElemInitContents(RefElem *e) {
  e->heapObject = nil();
  e->prev = NULL;
  e->next = NULL;
}

void refRegistryInitContents(RefRegistry *r) {
  r->used = NULL;
  r->free = NULL;
}

void refRegistryFreeContents(RefRegistry *r) {
  if (r != NULL) {
    RefElem *elem = NULL;

    elem = r->free;
    while (elem != NULL) {
      RefElem *freeMe = elem;
      elem = elem->next;
      free(freeMe);
    }
    r->free = NULL;

    elem = r->used;
    while (elem != NULL) {
      RefElem *freeMe = elem;
      elem = elem->next;
      free(freeMe);
    }
    r->used = NULL;
  }
}

Ref createRef(VM *vm, Value value) {

  RefRegistry *registry = &vm->refs;

  RefElem *newHead = NULL;
  if (registry->free != NULL) { // look for free first
    newHead = registry->free;
    registry->free = registry->free->next;
  }
  else { // allocate
    newHead = malloc(sizeof(RefElem));
    if (newHead == NULL) {
      explode("failed to allocate RefElem");
    }
  }

  newHead->heapObject = value;
  newHead->prev = NULL;
  newHead->next = registry->used;

  registry->used = newHead;

  Ref ref = (Ref)&newHead;
  return ref;
}

Value refDeref(Ref ref) {
  RefElem *elem = (RefElem*)ref;
  return elem->heapObject;
}

Ref getRefType(Ref ref) {
  RefElem *elem = (RefElem*)ref;
  return elem->heapObject.type;
}

void destroyRef(VM *vm, Ref ref) {

  RefRegistry *registry = &vm->refs;

  RefElem *elem = (RefElem*)ref;

  // clear
  elem->heapObject = nil();

  // remove
  if (elem->prev != NULL) {
    elem->prev->next = elem->next;
  }
  if (elem->next != NULL) {
    elem->next->prev = elem->prev;
  }

  // add to free list
  elem->prev = NULL;
  elem->next = registry->free;
  registry->free = elem;
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
  s->hash = 0;
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
  s->hash = 0;
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
  k->hash = 0;
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

RetVal tryMapHydrate(VM *vm, Value *alreadyHydratedConstants, MapConstant listConst, Value *value, Error *error) {
  RetVal ret;

  explode("not implemented");

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
    case CT_MAP:
      throws(tryMapHydrate(vm, alreadyHydratedConstants, c.map, &v, error));
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
RetVal tryVMPrn(VM *vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  exprInitContents(expr);

  TryPrn prn = vm->valueTypeTable.valueTypes[result.type].tryPrn;
  throws(prn(vm, result, pool, expr, error));

  return R_SUCCESS;

  failure:
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

#define ONE_KB 1024

RetVal tryVarInit(wchar_t *namespace, wchar_t *name, uint64_t symbolNameLength, Value value, Var *var, Error *error) {
  RetVal ret;

  // TODO: vars should be heap values, fix this leak!

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_KB, error));

  varInitContents(var);

  throws(tryCopyText(pool, namespace, &var->namespace, wcslen(namespace), error));
  throws(tryCopyText(pool, name, &var->name, symbolNameLength, error));
  var->value = value;

  ret = R_SUCCESS;
  goto done;

  failure:
  done:
    // TODO: vars should be heap values, fix this leak!
    // poolFree(pool);
    return ret;
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
  return ret;
}

RetVal tryNamespaceMake(wchar_t *name, uint64_t length, Namespace **ptr , Error *error) {

  RetVal ret;

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_KB, error));

  Namespace *ns;
  tryPalloc(pool, ns, sizeof(Namespace), "Namespace");

  // TODO: namespaces should be heap values! fix this leak!

  ns->name = NULL;
  ns->importedVars = NULL;
  ns->numImportedVars = 0;

  varListInit(&ns->localVars);

  throws(tryCopyText(pool, name, &ns->name, length, error));

  *ptr = ns;
  return R_SUCCESS;

  failure:
  return ret;
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

RetVal tryAllocateCons(VM *vm, ExecFrame_t frame, Value value, Value next, Value *ptr, Error *error) {
  RetVal ret;

  if (next.type != VT_NIL && next.type != VT_LIST) {
    throwRuntimeError(error, "a Cons next must be nil or a list: %s", getValueTypeName(vm, next.type));
  }

  Cons *cons = NULL;

  size_t size = sizeof(Cons);

  ptr->type = VT_LIST;
  cons = alloc(vm, frame, size, ptr);

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

      invocable->fn = deref(&vm->gc, invocable->fnRef);

      invocable->hasClosure = false;
      invocable->closure = NULL;
      break;
    }
    case VT_CLOSURE: {

      invocable->closure = deref(&vm->gc, invocable->fnRef);

      invocable->hasClosure = true;
      invocable->fnRef = invocable->closure->fn;
      invocable->fn = deref(&vm->gc, invocable->closure->fn);
      break;
    }
    case VT_CFN: {
      invocable->fn = deref(&vm->gc, invocable->fnRef);
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
        Cons *cons = deref(&vm->gc, seq);

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
      Cons *cons = deref(&vm->gc, seq);

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

  CFn *fn = deref(&vm->gc, cFn);
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

    ExecFrame_t parent = getParent(frame);

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

// TODO: understand why this algorithm works
// http://hg.openjdk.java.net/jdk7u/jdk7u6/jdk/file/8c2c5d63a17e/src/share/classes/java/lang/String.java
uint32_t stringHash(String *s) {
  uint32_t h = s->hash;
  if (h == 0 && s->length > 0) {
    wchar_t *val = stringValue(s);
    for (uint64_t i=0; i<s->length; i++) {
      h = 31 * h + val[i];
    }
    s->hash = h;
  }
  return h;
}

RetVal tryNilHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = 0;
  return R_SUCCESS;
}

RetVal tryUIntHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = value.value * 31;
  return R_SUCCESS;
}

RetVal tryBoolHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = value.value * 31;
  return R_SUCCESS;
}

RetVal tryFnHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = value.value * 31;
  return R_SUCCESS;
}

RetVal tryHashCode(VM *vm, Value value, uint32_t *hash, Error *error);

// TODO: understand why this works
RetVal tryListHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;

  Value seq = value;
  uint32_t h = 1;

  while (seq.type != VT_NIL) {

    Cons *cons = deref(&vm->gc, seq);

    uint32_t elemHash = 0;
    throws(tryHashCode(vm, cons->value, &elemHash, error));

    h = (31 * h) + elemHash;
    seq = cons->next;
  }

  *hash = h;
  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryClosureHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = value.value * 31;
  return R_SUCCESS;
}

RetVal tryCFnHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = value.value * 31;
  return R_SUCCESS;
}

RetVal tryArrayHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  *hash = value.value * 31;
  return R_SUCCESS;
}

RetVal tryMapEntryHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;

  MapEntry *entry = deref(&vm->gc, value);

  if (entry->hash == 0 && (entry->key.type != T_NIL || entry->value.type != T_NIL)) {

    uint32_t keyHash = 0;
    if (entry->key.type != T_NIL) {
      throws(tryHashCode(vm, entry->key, &keyHash, error));
    }

    uint32_t valueHash = 0;
    if (entry->value.type != T_NIL) {
      throws(tryHashCode(vm, entry->value, &valueHash, error));
    }

    entry->hash = keyHash + valueHash;
  }

  *hash = entry->hash;
  return R_SUCCESS;
  failure:
  return ret;
}

// TODO: understand why this works
RetVal tryMapHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;

  Map *map = deref(&vm->gc, value);

  Array *array = deref(&vm->gc, map->array);

  uint32_t h = 0;

  for (uint64_t i=0; i<array->length; i++) {

    Value e = arrayElements(array)[i];

    if (e.type == VT_NIL) {
      // empty
    }
    else if (e.type == VT_MAP_ENTRY) {
      uint32_t entryHash = 0;
      throws(tryHashCode(vm, e, &entryHash, error));
      h += entryHash;
    }
    else {
      throwRuntimeError(error, "map arrays may only contain nils and map-entries: %s", getValueTypeName(vm, e.type));
    }
  }

  *hash = h;
  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryStringHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;
  String *s = deref(&vm->gc, value);
  *hash = stringHash(s);
  return R_SUCCESS;
  failure:
    return ret;
}

uint32_t symbolHash(Symbol *s) {
  uint32_t h = s->hash;
  if (h == 0 && s->length > 0) {
    wchar_t *val = symbolValue(s);
    for (uint64_t i=0; i<s->length; i++) {
      h = 31 * h + val[i];
    }
    s->hash = h;
  }
  return h;
}

RetVal trySymbolHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;
  Symbol *s = deref(&vm->gc, value);
  *hash = symbolHash(s);
  return R_SUCCESS;
  failure:
  return ret;
}

uint32_t keywordHash(Keyword *s) {
  uint32_t h = s->hash;
  if (h == 0 && s->length > 0) {
    wchar_t *val = keywordValue(s);
    for (uint64_t i=0; i<s->length; i++) {
      h = 31 * h + val[i];
    }
    s->hash = h;
  }
  return h;
}

RetVal tryKeywordHashCode(VM_t vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;
  Keyword *s = deref(&vm->gc, value);
  *hash = keywordHash(s);
  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryHashCode(VM *vm, Value value, uint32_t *hash, Error *error) {
  RetVal ret;

  TryHashCode hashCode = vm->valueTypeTable.valueTypes[value.type].tryHashCode;
  if (hashCode == NULL) {
    throwRuntimeError(error, "hash not supported for type: %s", getValueTypeName(vm, value.type));
  }
  else {
    throws(hashCode(vm, value, hash, error));
  }

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

  if (a.type == b.type) {
    switch (a.type) {

      // these are equivalent based on type + value
      case VT_NIL:
      case VT_UINT:
      case VT_BOOL:
      case VT_FN:
      case VT_CLOSURE:
      case VT_CFN:
      case VT_LIST:
        c.value = a.value == b.value;
        break;

      // these implement hashcode
      case VT_STR:
      case VT_SYMBOL:
      case VT_KEYWORD: {
        if (a.value == b.value) {
          c.value = true;
        }
        else {
          uint32_t hashA = 0, hashB = 0;
          throws(tryHashCode(vm, a, &hashA, error));
          throws(tryHashCode(vm, b, &hashB, error));
          c.value = hashA == hashB;
        }
        break;
      }

      default:
        explode("Unhandled");
    }
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

  String *str = deref(&vm->gc, varName);

  throws(tryDefVar(&vm->namespaces, stringValue(str), str->length, value, error));

  // define always returns nil
  Value result = nil();
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

  String *str = deref(&vm->gc, varName);

  Var *var = NULL;
  if (!resolveVar(&vm->namespaces, stringValue(str), str->length, &var)) {
    // fail: not all vars exist
    throwRuntimeError(error, "no such var found: '%ls'", stringValue(str));
  }
  // TODO: don't let users take the value of a macro, per clojure? could do this by making a special macro invoke instruction/builtin
//  else if (var->isMacro) {
//    throwRuntimeError(error, "cannot take the value of a macro: '%ls/%ls'", var->namespace, var->name);
//  }
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

  Fn *fn = deref(&vm->gc, fnValue);

  Closure *closure = NULL;

  size_t capturesSize = fn->numCaptures * sizeof(Value);
  size_t clSize = sizeof(Closure) + capturesSize;

  Value closureValue;
  closureValue.type = VT_CLOSURE;

  closure = alloc(vm, frame, clSize, &closureValue);

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

  Cons *cons = deref(&vm->gc, result);
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
    Cons *cons = deref(&vm->gc, seq);
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
    Cons *cons = deref(&vm->gc, seq);
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
    String *str = deref(&vm->gc, strValue);
    sym = stringValue(str);
    symLength = str->length;
  }
  else if (strValue.type == VT_SYMBOL) {
    Symbol *s = deref(&vm->gc, strValue);
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
    String *str = deref(&vm->gc, strValue);
    sym = stringValue(str);
    symLength = str->length;
  }
  else if (strValue.type == VT_SYMBOL) {
    Symbol *s = deref(&vm->gc, strValue);
    sym = symbolValue(s);
    symLength = s->length;
  }
  else {
    throwRuntimeError(error, "only strings or symbols can identify vars: %s", getValueTypeName(vm, strValue.type));
  }

  String *str = deref(&vm->gc, strValue);

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

  collect(vm, frame);

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

RetVal tryVMPrn(VM *vm, Value result, Pool_t pool, Expr *expr, Error *error);

#define ONE_KB 1024

// (8),             | (value -> value)
RetVal tryPrnEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_KB, error));

  Value value;
  throws(popOperand(frame, &value, error));

  Expr expr;
  throws(tryVMPrn(vm, value, pool, &expr, error));
  throws(tryExprPrn(pool, &expr, error));
  printf("\n");

  throws(pushOperand(frame, nil(), error));

  ret = R_SUCCESS;
  goto done;

  failure:
    goto done;

  done:
    poolFree(pool);
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

void relocateChildrenFn(VM_t vm, void *oldHeap, void *obj) {
  Fn *fn = obj;
  for (uint16_t i=0; i<fn->numConstants; i++) {
    relocate(vm, oldHeap, &fnConstants(fn)[i]);
  }
}

void relocateChildrenList(VM_t vm, void *oldHeap, void *obj) {
  Cons *cons = obj;
  relocate(vm, oldHeap, &cons->value);
  relocate(vm, oldHeap, &cons->next);
}

void relocateChildrenClosure(VM_t vm, void *oldHeap, void *obj) {
  Closure *closure = obj;
  relocate(vm, oldHeap, &closure->fn);
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    relocate(vm, oldHeap, &closureCaptures(closure)[i]);
  }
}

void relocateChildrenArray(VM_t vm, void *oldHeap, void *obj) {
  Array *arr = obj;
  for (uint16_t i=0; i<arr->length; i++) {
    relocate(vm, oldHeap, &arrayElements(arr)[i]);
  }
}

void relocateChildrenMap(VM_t vm, void *oldHeap, void *obj) {
  Map *map = obj;
  relocate(vm, oldHeap, &map->array);
}

void relocateChildrenMapEntry(VM_t vm, void *oldHeap, void *obj) {
  MapEntry *mapEntry = obj;
  relocate(vm, oldHeap, &mapEntry->key);
  relocate(vm, oldHeap, &mapEntry->value);
}

RetVal tryPrnNil(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_NIL;
  return R_SUCCESS;
}

RetVal tryPrnInt(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_NUMBER;
  expr->number.value = result.value;
  return R_SUCCESS;
}

RetVal tryPrnBool(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_BOOLEAN;
  expr->boolean.value = result.value;
  return R_SUCCESS;
}

RetVal tryPrnFn(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<function>";
  expr->string.length = wcslen(function);
  return tryCopyText(pool, function, &expr->string.value, expr->string.length, error);
}

RetVal tryPrnCFn(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<c-function>";
  expr->string.length = wcslen(function);
  return tryCopyText(pool, function, &expr->string.value, expr->string.length, error);
}

RetVal tryPrnClosure(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<closure>";
  expr->string.length = wcslen(function);
  return tryCopyText(pool, function, &expr->string.value, expr->string.length, error);
}

RetVal equalsString(VM *vm, Value value, wchar_t *cmpStr, bool *equals, Error *error) {
  RetVal ret;

  *equals = false;
  if (value.type == VT_STR) {

    String *str = deref(&vm->gc, value);

    if (wcscmp(stringValue(str), cmpStr) == 0) {
      *equals = true;
    }
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrnStr(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  String *str = deref(&vm->gc, result);

  expr->type = N_STRING;
  expr->string.length = str->length;
  throws(tryCopyText(pool, stringValue(str), &expr->string.value, expr->string.length, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrnSymbol(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {

  RetVal ret;

  Symbol *sym = deref(&vm->gc, result);

  expr->type = N_SYMBOL;
  expr->symbol.length = sym->length;
  throws(tryCopyText(pool, symbolValue(sym), &expr->symbol.value, expr->string.length, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrnKeyword(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  Keyword *kw = deref(&vm->gc, result);

  expr->type = N_KEYWORD;
  expr->keyword.length = kw->length;
  throws(tryCopyText(pool, keywordValue(kw), &expr->keyword.value, expr->string.length, error));

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

  Cons *properties = deref(&vm->gc, *ptr);

  if (properties->value.type != VT_KEYWORD) {
    throwRuntimeError(error, "expected keyword for property key: %s",
                      getValueTypeName(vm, properties->value.type));
  }

  p->key = deref(&vm->gc, properties->value);

  if (isEmpty(properties->next)) {
    throwRuntimeError(error, "expected value for property but only found a key: %ls", keywordValue(p->key));
  }

  properties = deref(&vm->gc, properties->next);
  p->value = properties->value;

  *ptr = properties->next;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryPrnMetadata(VM_t vm, Value metadata, Expr *expr, Error *error) {
  RetVal ret;

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

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrnList(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  Cons *cons = deref(&vm->gc, result);

  expr->type = N_LIST;

  throws(tryPrnMetadata(vm, cons->header.metadata, expr, error));

  listInitContents(&expr->list);
  Expr *elem;

  tryPalloc(pool, elem, sizeof(Expr), "Expr");
  exprInitContents(elem);

  throws(tryVMPrn(vm, cons->value, pool, elem, error));
  throws(tryListAppend(pool, &expr->list, elem, error));

  while (cons->next.type != VT_NIL) {

    if (cons->next.type != VT_LIST) {
      throwRuntimeError(error, "this should always be a type of VT_LIST: %s",
                        getValueTypeName(vm, cons->next.type));
    }

    cons = deref(&vm->gc, cons->next);

    tryPalloc(pool, elem, sizeof(Expr), "Expr");
    exprInitContents(elem);

    throws(tryVMPrn(vm, cons->value, pool, elem, error));
    throws(tryListAppend(pool, &expr->list, elem, error));
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrnArray(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_STRING;
  wchar_t function[] = L"<array>";
  expr->string.length = wcslen(function);
  return tryCopyText(pool, function, &expr->string.value, expr->string.length, error);
}

RetVal tryPrnMap(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  Map *map = deref(&vm->gc, result);

  Array *array = deref(&vm->gc, map->array);

  expr->type = N_MAP;
  throws(tryPrnMetadata(vm, map->header.metadata, expr, error));

  mapInitContents(&expr->map);

  for (uint64_t i=0; i<array->length; i++) {
    Value entryRef = arrayElements(array)[i];

    if (entryRef.type == VT_MAP_ENTRY) {

      MapEntry *entry = deref(&vm->gc, entryRef);

      Expr *keyExpr = NULL;
      tryPalloc(pool, keyExpr, sizeof(Expr), "Expr");
      exprInitContents(keyExpr);
      throws(tryVMPrn(vm, entry->key, pool, keyExpr, error));

      Expr *valueExpr = NULL;
      tryPalloc(pool, valueExpr, sizeof(Expr), "Expr");
      exprInitContents(valueExpr);
      throws(tryVMPrn(vm, entry->value, pool, valueExpr, error));

      throws(tryMapPut(pool, &expr->map, keyExpr, valueExpr, error));
    }
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryEqualsNil(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = that.type == T_NIL;
  return R_SUCCESS;
}

RetVal tryEqualsUInt(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = this.type == that.type && this.value == that.value;
  return R_SUCCESS;
}

RetVal tryEqualsBool(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = this.type == that.type && this.value == that.value;
  return R_SUCCESS;
}

RetVal tryEqualsFn(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = this.type == that.type && this.value == that.value;
  return R_SUCCESS;
}

RetVal tryEqualsStr(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  RetVal ret;

  if (this.type != that.type) {
    *equal = false;
  }
  else if (this.value == that.value) {
    *equal = true;
  }
  else {

    String *a = deref(&vm->gc, this);
    String *b = deref(&vm->gc, that);

    if (a->length != b->length) {
      *equal = false;
    }
    else {
      *equal = wcscmp(stringValue(a), stringValue(b)) == 0;
    }
  }

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryEqualsSymbol(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  RetVal ret;

  if (this.type != that.type) {
    *equal = false;
  }
  else if (this.value == that.value) {
    *equal = true;
  }
  else {

    Symbol *a = deref(&vm->gc, this);
    Symbol *b = deref(&vm->gc, that);

    if (a->length != b->length) {
      *equal = false;
    }
    else {
      *equal = wcscmp(symbolValue(a), symbolValue(b)) == 0;
    }
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryEqualsKeyword(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  RetVal ret;

  if (this.type != that.type) {
    *equal = false;
  }
  else if (this.value == that.value) {
    *equal = true;
  }
  else {

    Keyword *a = deref(&vm->gc, this);
    Keyword *b = deref(&vm->gc, that);

    if (a->length != b->length) {
      *equal = false;
    }
    else {
      *equal = wcscmp(keywordValue(a), keywordValue(b)) == 0;
    }
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryEquals(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  RetVal ret;

  TryEquals tryEquals = vm->valueTypeTable.valueTypes[this.type].tryEquals;
  if (tryEquals == NULL) {
    throwRuntimeError(error, "equals not supported for type: %s", getValueTypeName(vm, this.type));
  }
  else {
    throws(tryEquals(vm, this, that, equal, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryEqualsList(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  RetVal ret;

  if (this.type != that.type) {
    *equal = false;
  }
  else if (this.value == that.value) {
    *equal = true;
  }
  else {

    bool e = true;

    while (this.type != T_NIL) {

      Cons *a = deref(&vm->gc, this);
      Cons *b = deref(&vm->gc, that);

      bool elementEqual = false;
      throws(tryEquals(vm, a->value, b->value, &elementEqual, error));
      if (!elementEqual) {
        e = false;
        break;
      }

      bool nextTypesMismatched = a->next.type != b->next.type;
      if (nextTypesMismatched) {
        e = false;
        break;
      }

      this = a->next;
      that = b->next;
    }

    *equal = e;
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryEqualsClosure(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = this.type == that.type && this.value == that.value;
  return R_SUCCESS;
}

RetVal tryEqualsCFn(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = this.type == that.type && this.value == that.value;
  return R_SUCCESS;
}

RetVal tryEqualsArray(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  *equal = this.type == that.type && this.value == that.value;
  return R_SUCCESS;
}

RetVal tryEqualsMap(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  if (this.type == that.type && this.value == that.value) {
    *equal = true;
  }
  else {
    // TODO
    *equal = false;
  }
  return R_SUCCESS;
}

RetVal tryEqualsMapEntry(VM_t vm, Value this, Value that, bool *equal, Error *error) {
  RetVal ret;

  if (this.type != that.type) {
    *equal = false;
  }
  else if (this.value == that.value) {
    *equal = true;
  }
  else {
    MapEntry *a = deref(&vm->gc, this);
    MapEntry *b = deref(&vm->gc, that);

    bool keyEqual = false;
    throws(tryEquals(vm, a->key, b->key, &keyEqual, error));

    if (!keyEqual) {
      *equal = false;
    }
    else {
      bool valueEqual = false;
      throws(tryEquals(vm, a->value, b->value, &valueEqual, error));
      *equal = valueEqual;
    }
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
    table.valueTypes[i].relocateChildren = NULL;
    table.valueTypes[i].tryPrn = NULL;
  }

  // init with known value types
  ValueTypeInfo valueTypes [] = {
      [VT_NIL]       = {.name = "nil",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyNo,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnNil,
                        .tryHashCode = &tryNilHashCode,
                        .tryEquals = &tryEqualsNil,},
      [VT_UINT]      = {.name = "uint",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnInt,
                        .tryHashCode = &tryUIntHashCode,
                        .tryEquals = &tryEqualsUInt},
      [VT_BOOL]      = {.name = "bool",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyBool,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnBool,
                        .tryHashCode = &tryBoolHashCode,
                        .tryEquals = &tryEqualsBool},
      [VT_FN]        = {.name = "fn",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenFn,
                        .tryPrn = &tryPrnFn,
                        .tryHashCode = &tryFnHashCode,
                        .tryEquals = &tryEqualsFn},
      [VT_STR]       = {.name = "str",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnStr,
                        .tryHashCode = &tryStringHashCode,
                        .tryEquals = &tryEqualsStr},
      [VT_SYMBOL]    = {.name = "symbol",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnSymbol,
                        .tryHashCode = &trySymbolHashCode,
                        .tryEquals = &tryEqualsSymbol},
      [VT_KEYWORD]   = {.name = "keyword",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnKeyword,
                        .tryHashCode = &tryKeywordHashCode,
                        .tryEquals = &tryEqualsKeyword},
      [VT_LIST]      = {.name = "list",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenList,
                        .tryPrn = &tryPrnList,
                        .tryHashCode = &tryListHashCode,
                        .tryEquals = &tryEqualsList},
      [VT_CLOSURE]   = {.name = "closure",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenClosure,
                        .tryPrn = &tryPrnClosure,
                        .tryHashCode = &tryClosureHashCode,
                        .tryEquals = &tryEqualsClosure},
      [VT_CFN]       = {.name = "cfn",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnCFn,
                        .tryHashCode = &tryCFnHashCode,
                        .tryEquals = &tryEqualsCFn},
      [VT_ARRAY]     = {.name = "array",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenArray,
                        .tryPrn = &tryPrnArray,
                        .tryHashCode = &tryArrayHashCode,
                        .tryEquals = &tryEqualsArray},
      [VT_MAP]       = {.name = "map",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenMap,
                        .tryPrn = &tryPrnMap,
                        .tryHashCode = &tryMapHashCode,
                        .tryEquals = &tryEqualsMap},
      [VT_MAP_ENTRY] = {.name = "map-entry",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenMapEntry,
                        .tryPrn = NULL,
                        .tryHashCode = &tryMapEntryHashCode,
                        .tryEquals = &tryEqualsMapEntry},
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
      current = getParent(current);
    }
  }
}

RetVal tryExceptionMake(ExecFrame_t frame, Pool_t pool, VMException *exception, Error *error) {
  RetVal ret;

  Error reference = *error;

  wchar_t msg[ERROR_MSG_LENGTH];
  exceptionInitContents(exception);

  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", reference.message);
  throws(tryTextMake(pool, msg, &exception->message, wcslen(msg), error));

  uint64_t numFrames = 0;
  {
    ExecFrame_t current = frame;
    while (true) {
      numFrames++;
      if (!hasParent(current)) {
        break;
      }
      else {
        current = getParent(current);
      }
    }
  }

  // native frame
  numFrames++;

  exception->frames.length = numFrames;
  tryPalloc(pool, exception->frames.elements, sizeof(VMExceptionFrame) * numFrames, "VMExceptionFrame array");

  { // native frame

    VMExceptionFrame *f = &exception->frames.elements[0];
    exFrameInitContents(f);

    f->functionName.length = strlen(reference.functionName) + 1;
    tryPalloc(pool, f->functionName.value, f->functionName.length * sizeof(wchar_t), "wide string");
    swprintf(f->functionName.value, f->functionName.length, L"%s", reference.functionName);

    f->unknownSource = false;

    char* fileName = basename((char *) reference.fileName);
    f->fileName.length = strlen(fileName) + 1;
    tryPalloc(pool, f->fileName.value, f->fileName.length * sizeof(wchar_t), "wide string");
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
      throws(tryTextCopy(pool, &text, &f->functionName, error));
    }
    else {
      wchar_t *name = L"<root>\0";
      throws(tryTextMake(pool, name, &f->functionName, wcslen(name), error));
    }

    if (hasSourceTable(current)) {
      f->unknownSource = false;
      getFileName(current, &f->fileName);
      getLineNumber(current, &f->lineNumber);
    }

    if (hasParent(current)) {
      current = getParent(current);
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryExceptionPrint(Pool_t pool, VMException *e, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(pool, &b, error));

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
  throws(tryCopyText(pool, stringBufferText(b), &output, stringBufferLength(b), error));

  *ptr = output;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryExceptionPrintf(VMException *e, Error *error) {
  RetVal ret;

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_KB, error));

  wchar_t *msg;
  throws(tryExceptionPrint(pool, e, &msg, error));
  printf("%ls\n", msg);

  ret = R_SUCCESS;
  goto done;

  failure:
  done:
    poolFree(pool);
    return ret;
}

RetVal tryFrameEval(VM *vm, ExecFrame_t frame, Pool_t outputPool, Error *error) {
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
        parent = getParent(frame);
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
      // TODO: exceptions should go on the heap
      throws(tryExceptionMake(frame, outputPool, &ex, error));
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
  Fn *fn;

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

Value* getLocalRef(ExecFrame *frame, uint16_t localIndex) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  return &frame->locals[localIndex];
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

Value* getOperandRef(ExecFrame *frame, uint64_t opIndex) {
  if (opIndex >= frame->opStack->usedDepth) {
    explode("no such operand: %" PRIu64, opIndex);
  }
  return &frame->opStack->stack[opIndex];
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

void setFnRef(VM *vm, ExecFrame *frame, Value value) {
  frame->fnRef = value;
  frame->fn = deref(&vm->gc, value);
}

bool hasResult(ExecFrame *frame) {
  return frame->resultAvailable;
}

bool hasParent(ExecFrame *frame) {
  return frame->parent != NULL;
}

ExecFrame_t getParent(ExecFrame *frame) {
  if (frame->parent == NULL) {
    explode("no parent available");
  }
  return frame->parent;
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

  Fn *fn = deref(&vm->gc, newFn);

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

  Fn *fn = deref(&vm->gc, newFn);

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

RetVal _tryVMEval(VM *vm, CodeUnit *codeUnit, Pool_t outputPool, Value *result, VMException *exception, bool *exceptionThrown,  Error *error) {

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

  throws(tryFrameEval(vm, frame, outputPool, error));

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

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, Pool_t outputPool, VMEvalResult *result, Error *error) {

  RetVal ret;

  Value value;
  VMException exception;
  bool exceptionThrown = false;

  throws(_tryVMEval(vm, codeUnit, outputPool, &value, &exception, &exceptionThrown, error));

  if (exceptionThrown) {
    result->type = RT_EXCEPTION;
    result->exception = exception;
  }
  else {
    result->type = RT_RESULT;
    throws(tryVMPrn(vm, value, outputPool, &result->result, error));
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

#define ASSERT_SYM(value, ...) {\
  if (value.type != VT_SYMBOL) { \
    throwRuntimeError(error, "expected a symbol type: %s", getValueTypeName(vm, value.type)); \
  } \
}

RetVal tryStringMakeBlank(VM *vm, ExecFrame_t frame, uint64_t length, Value *value, Error *error) {
  RetVal ret;

  String *str = NULL;

  size_t textSize = (length + 1) * sizeof(wchar_t);
  size_t strSize = sizeof(String) + textSize;

  value->type = VT_STR;
  str = alloc(vm, frame, strSize, value);

  stringInitContents(str);
  str->header.type = VT_STR;
  str->header.size = strSize;
  str->length = length;

  str->valueOffset = sizeof(String);
  stringValue(str)[length] = L'\0';

  return R_SUCCESS;
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

    Cons *seq = deref(&vm->gc, cursor);

    ASSERT_STR(seq->value);

    String *string = deref(&vm->gc, seq->value);
    totalLength += string->length;

    cursor = seq->next;
  }

  // store the list on the op stack while we allocate since gc may happen
  throws(pushOperand(frame, strings, error));

  Value resultRef = nil();
  throws(tryStringMakeBlank(vm, frame, totalLength, &resultRef, error));
  String *result = deref(&vm->gc, resultRef);

  // get the list back again after allocation
  throws(popOperand(frame, &strings, error));

  uint64_t totalSizeWritten = 0;

  cursor = strings;
  while (cursor.type != VT_NIL) {
    Cons *seq = deref(&vm->gc, cursor);

    String *string = deref(&vm->gc, seq->value);

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
  Pool_t pool = NULL;
  Expr expr;
  exprInitContents(&expr);
  StringBuffer_t b = NULL;

  throws(tryPoolCreate(&pool, ONE_KB, error));

  throws(tryVMPrn(vm, value, pool, &expr, error));
  throws(tryStringBufferMake(pool, &b, error));
  throws(tryExprPrnBufConf(&expr, b, readable, error));

  Value resultRef = nil();
  throws(tryStringMakeBlank(vm, frame, stringBufferLength(b), &resultRef, error));
  String *result = deref(&vm->gc, resultRef);

  memcpy(stringValue(result), stringBufferText(b), stringBufferLength(b) * sizeof(wchar_t));

  throws(pushOperand(frame, resultRef, error));

  ret = R_SUCCESS;
  goto done;

  failure:
  goto done;

  done:
    // clean up off-heap memory
    poolFree(pool);
    return ret;
}

RetVal tryPrStrBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  return tryPrStrBuiltinConf(vm, frame, true, error);
}

RetVal tryPrintStrBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  return tryPrStrBuiltinConf(vm, frame, false, error);
}

RetVal trySymbolMakeBlank(VM *vm, ExecFrame_t frame, uint64_t length, Value *result, Error *error) {
  RetVal ret;

  Symbol *sym = NULL;

  size_t textSize = (length + 1) * sizeof(wchar_t);
  size_t size = sizeof(Symbol) + textSize;

  result->type = VT_SYMBOL;
  sym = alloc(vm, frame, size, result);

  symbolInitContents(sym);
  sym->header.type = VT_SYMBOL;
  sym->header.size = size;
  sym->length = length;

  sym->valueOffset = sizeof(Symbol);
  symbolValue(sym)[sym->length] = L'\0';

  return R_SUCCESS;
}

RetVal trySymbolBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  throws(popOperand(frame, &value, error));

  ASSERT_STR(value);

  uint64_t length = 0;
  {
    String *string = deref(&vm->gc, value);
    length = string->length;
  }

  // keep the string safely on the op stack while we allocate
  throws(pushOperand(frame, value, error));

  Value result;
  throws(trySymbolMakeBlank(vm, frame, length, &result, error));

  // pop string back off now we're done allocating
  throws(popOperand(frame, &value, error));

  // actually copy string into symbol
  String *string = deref(&vm->gc, value);
  Symbol *sym = deref(&vm->gc, result);
  memcpy(symbolValue(sym), stringValue(string), sym->length * sizeof(wchar_t));

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryKeywordMakeBlank(VM *vm, ExecFrame_t frame, uint64_t length, Value *result, Error *error) {
  Keyword *kw = NULL;

  size_t textSize = (length + 1) * sizeof(wchar_t);
  size_t size = sizeof(Keyword) + textSize;

  result->type = VT_KEYWORD;
  kw = alloc(vm, frame, size, result);

  keywordInitContents(kw);
  kw->header.type = VT_KEYWORD;
  kw->header.size = size;
  kw->length = length;

  kw->valueOffset = sizeof(Symbol);
  keywordValue(kw)[kw->length] = L'\0';

  return R_SUCCESS;
}

RetVal tryKeywordBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  throws(popOperand(frame, &value, error));

  ASSERT_STR(value);

  uint64_t length = 0;
  {
    String *string = deref(&vm->gc, value);
    length = string->length;
  }

  // keep the string safely on the op stack while we allocate
  throws(pushOperand(frame, value, error));

  Value result;
  throws(tryKeywordMakeBlank(vm, frame, length, &result, error));

  // pop string back off now we're done allocating
  throws(popOperand(frame, &value, error));

  // actually copy string into symbol
  String *string = deref(&vm->gc, value);
  Keyword *kw = deref(&vm->gc, result);
  memcpy(keywordValue(kw), stringValue(string), kw->length * sizeof(wchar_t));

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;
  failure:
  return ret;
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

void arrayInitContents(Array *a) {
  objectHeaderInitContents(&a->header);
  a->length = 0;
  a->elementsOffset = 0;
}

RetVal tryMakeArray(VM *vm, ExecFrame_t frame, uint64_t length, Value *value, Error *error) {
  Array *arr = NULL;

  size_t elementsSize = length * sizeof(Value);
  size_t size = sizeof(Array) + elementsSize;

  value->type = VT_ARRAY;
  arr = alloc(vm, frame, size, value);

  arrayInitContents(arr);
  arr->header.type = VT_ARRAY;
  arr->header.size = size;
  arr->length = length;
  arr->elementsOffset = sizeof(Array);

  Value *elements = arrayElements(arr);
  Value init = nil();
  for (uint64_t i=0; i<length; i++) {
    elements[i] = init;
  }

  return R_SUCCESS;
}

void _mapInitContents(Map *m) {
  objectHeaderInitContents(&m->header);
  m->array = nil();
}

#define MIN_MAP_BUCKETS 16

RetVal tryMakeMapConf(VM *vm, ExecFrame_t frame, Value *value, Error *error) {
  RetVal ret;

  {
    Value array;
    throws(tryMakeArray(vm, frame, MIN_MAP_BUCKETS, &array, error));
    throws(pushOperand(frame, array, error));
  }

  Map *map = NULL;

  size_t size = sizeof(Map);

  value->type = VT_MAP;
  map = alloc(vm, frame, size, value);

  _mapInitContents(map);
  map->header.type = VT_MAP;
  map->header.size = size;

  throws(popOperand(frame, &map->array, error));
  map->size = 0;

  return R_SUCCESS;

  failure:
  return ret;
}

/*
 * TODO: add a 'suspend gc' feature, so that c functions can prevent it from happening while doing critical stuff
 * - having two different models for allocation sucks, it duplicates a lot of code (hydration vs execution)
 * - having to register things in the op stack is noisy and error prone
 */

RetVal tryMakeMap(VM *vm, ExecFrame_t frame, Value *value, Error *error) {
  RetVal ret;

  {
    Value array;
    throws(tryMakeArray(vm, frame, MIN_MAP_BUCKETS, &array, error));
    throws(pushOperand(frame, array, error));
  }

  Map *map = NULL;

  size_t size = sizeof(Map);

  value->type = VT_MAP;
  map = alloc(vm, frame, size, value);

  _mapInitContents(map);
  map->header.type = VT_MAP;
  map->header.size = size;

  throws(popOperand(frame, &map->array, error));
  map->size = 0;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryHashMapBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  throws(tryMakeMap(vm, frame, &value, error));

  throws(pushOperand(frame, value, error));

  return R_SUCCESS;
  failure:
  return ret;
}

void mapEntryInitContents(MapEntry *e) {
  objectHeaderInitContents(&e->header);
  e->key = nil();
  e->value = nil();
  e->hash = 0;
}

RetVal tryMakeMapEntry(VM *vm, ExecFrame_t frame, Value *value, Error *error) {
  MapEntry *entry = NULL;

  size_t size = sizeof(MapEntry);

  value->type = VT_MAP_ENTRY;
  entry = alloc(vm, frame, size, value);

  mapEntryInitContents(entry);
  entry->header.type = VT_MAP_ENTRY;
  entry->header.size = size;

  return R_SUCCESS;
}

#define MIN_LOAD .40
#define MAX_LOAD .70

RetVal tryPutMapBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value map, key, value;

  throws(popOperand(frame, &value, error));
  throws(popOperand(frame, &key, error));
  throws(popOperand(frame, &map, error));

  uint32_t hash = 0;
  throws(tryHashCode(vm, key, &hash, error));

  bool putHappened = false;
  bool makeNewEntry = false;
  uint64_t newEntryIndex = 0;
  {
    Map *m = deref(&vm->gc, map);

    Array *array = deref(&vm->gc, m->array);

    uint64_t index = hash % array->length;

    for (uint64_t i = index; i < array->length; i++) {

      Value bucket = arrayElements(array)[i];

      if (bucket.type == VT_NIL) {
        makeNewEntry = true;
        newEntryIndex = i;
        break;
      }

      MapEntry *entry = deref(&vm->gc, bucket);

      bool keyEqual = false;
      throws(tryEquals(vm, key, entry->key, &keyEqual, error));

      if (keyEqual) {
        // found a match, replace the value
        entry->value = value;
        putHappened = true;
        break;
      }
    }
  }

  if (putHappened) {
    // great
  }
  else if (makeNewEntry) {

    // keep args safe from gc
    throws(pushOperand(frame, map, error));
    throws(pushOperand(frame, key, error));
    throws(pushOperand(frame, value, error));

    // may cause gc
    Value entryRef;
    throws(tryMakeMapEntry(vm, frame, &entryRef, error));

    throws(popOperand(frame, &value, error));
    throws(popOperand(frame, &key, error));
    throws(popOperand(frame, &map, error));

    Map *m = deref(&vm->gc, map);

    Array *array = deref(&vm->gc, m->array);

    arrayElements(array)[newEntryIndex] = entryRef;

    MapEntry *entry = deref(&vm->gc, entryRef);

    entry->key = key;
    entry->value = value;
    entry->hash = hash;

    m->size += 1;
  }
  else {
    throwRuntimeError(error, "could not find unused or equivalent key slot for value");
  }

  /*
   * TODO: can we tweak alloc() to let us allocate a whole set of map entries in one go?
   * this would simplify rehashing in terms of avoiding the gc only once
   *
   * TODO: is there an easy way to protect builtin arguments from relocation in gc?
   * - could keep a reference to them explicitly in the stack frame or some such
   * - also, could prevent allocation when c functions are running
   * both ideas have problems
   */

  float load = 0;
  uint64_t arraySize = 0;
  {
    Map *m = deref(&vm->gc, map);

    Array *array = deref(&vm->gc, m->array);

    arraySize = array->length;
    load = (float)m->size / (float)array->length;
  }
 /*
(do
  (def x (hash-map))
  (assoc x "one" "two"))
  */

  if (load > MAX_LOAD || (load > MIN_MAP_BUCKETS && load < MIN_LOAD)) {

    uint64_t newArraySize;
    if (load > MAX_LOAD) {
      newArraySize = arraySize * 2;
    }
    else {
      newArraySize = arraySize / 2;
      if (newArraySize < MIN_MAP_BUCKETS) {
        newArraySize = MIN_MAP_BUCKETS;
      }
    }

    // make new array
    throws(pushOperand(frame, map, error));
    Value newArrayRef;
    throws(tryMakeArray(vm, frame, newArraySize, &newArrayRef, error));
    throws(popOperand(frame, &map, error));

    // reuse the existing map entries

    Map *m = deref(&vm->gc, map);

    Array *oldArray = deref(&vm->gc, m->array);

    Array *newArray = deref(&vm->gc, newArrayRef);

    for (uint64_t i=0; i<oldArray->length; i++) {
      Value rehomeRef = arrayElements(oldArray)[i];

      if (rehomeRef.type == VT_NIL) {
        continue;
      }

      uint64_t index;
      {
        MapEntry *rehomeEntry = deref(&vm->gc, rehomeRef);

        index = rehomeEntry->hash % newArray->length;
      }
      bool rehomed = false;

      for (uint64_t j = index; j < newArray->length; j++) {
        Value *newHome = &arrayElements(newArray)[j];

        if (newHome->type == VT_NIL) {
          *newHome = rehomeRef;
          rehomed = true;
          break;
        }
      }

      if (!rehomed) {
        throwRuntimeError(error, "failed to rehome");
      }
    }

    m->array = newArrayRef;
  }

  throws(pushOperand(frame, map, error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryGetMapBuiltin(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value map, key;

  throws(popOperand(frame, &key, error));
  throws(popOperand(frame, &map, error));

  uint32_t hash = 0;
  throws(tryHashCode(vm, key, &hash, error));

  Value foundValue = nil();
  {
    Map *m = deref(&vm->gc, map);

    Array *array = deref(&vm->gc, m->array);

    uint64_t index = hash % array->length;

    for (uint64_t i = index; i < array->length; i++) {

      Value bucket = arrayElements(array)[i];

      if (bucket.type == VT_NIL) {
        break;
      }

      MapEntry *entry = deref(&vm->gc, bucket);

      bool keyEqual = false;
      throws(tryEquals(vm, key, entry->key, &keyEqual, error));

      if (keyEqual) {
        foundValue = entry->value;
        break;
      }
    }
  }

  throws(pushOperand(frame, foundValue, error));

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
  throws(tryDefineCFn(vm, L"eq",        2, false, tryCmpEval,          error));
  throws(tryDefineCFn(vm, L"join",      1, false, tryStrJoinBuiltin,   error));
  throws(tryDefineCFn(vm, L"pr-str",    1, false, tryPrStrBuiltin,     error));
  throws(tryDefineCFn(vm, L"print-str", 1, false, tryPrintStrBuiltin,  error));
  throws(tryDefineCFn(vm, L"symbol",    1, false, trySymbolBuiltin,    error));
  throws(tryDefineCFn(vm, L"keyword",   1, false, tryKeywordBuiltin,   error));
  throws(tryDefineCFn(vm, L"hash-map",  0, false, tryHashMapBuiltin,   error));
  throws(tryDefineCFn(vm, L"assoc",     3, false, tryPutMapBuiltin,    error));
  throws(tryDefineCFn(vm, L"get",       2, false, tryGetMapBuiltin,    error));

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryVMInitContents(VM *vm , Error *error) {
  RetVal ret;

  vm->instTable = instTableCreate();
  vm->valueTypeTable = valueTypeTableCreate();
  GCCreate(&vm->gc, 1024 * 1000);
  throws(tryNamespacesInitContents(&vm->namespaces, error));
  throws(tryInitCFns(vm, error));
  refRegistryInitContents(&vm->refs);

  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFreeContents(VM *vm) {
  if (vm != NULL) {
    GCFreeContents(&vm->gc);
    refRegistryFreeContents(&vm->refs);
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
 * TODO: the suffering I'm encountering is because unlike the JVM, I'm directly interacting with gc'd memory rather
 * than interacting with it through a gc-proof layer.
 *
 * Ideally, I'd define objects generically, with a predefined number of 'slots' for fields.
 * I could even embed the number of slots at the front of the object, so the object could be traversed
 * by gc automatically.
 *
 * I'd make defines to describe field name -> field number, and use them to get/set field values.
 * I'd need special array support. The new world would be arrays, objects, and primitives.
 *
 * I'd still need native support for defining functions, though the constants could be built up in an array and referenced.
 *
 *
 * In my actual c functions, I'd still need a way to alias my objects such that I'm not holding direct references
 * to them. I could make a JNI-like registry for each c function call, and expose an API to the c functions that honors
 * the ids from that registry.
 *
 * *But the upshot of this is that my c code is largely library code, and I'm writing it on the wrong side of the fence.*
 */



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
