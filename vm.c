#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>
#include "vm.h"
#include "utils.h"


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

typedef struct Frame *Frame_t;
typedef RetVal (*CFnInvoke) (VM_t vm, Frame_t frame, Error *error);

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

typedef RetVal (*TryEval) (struct VM *vm, Frame_t frame, Error *error);

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

typedef struct StackSegment StackSegment;

typedef struct StackSegment {
  void *data;           // the first valid address in the segment
  void *dataEnd;        // the first address after the end of the segment
  void *allocPtr;       // the offset to use for allocation
  StackSegment *prev;        // the previous segment, may be NULL
  StackSegment *next;        // the subsequent segment, may be NULL
} StackSegment;

typedef struct Stack {
  uint64_t segmentSize; // the size of each allocated segment (configuration)
  StackSegment *root;        // the first segment to be allocated
  StackSegment *current;     // the most recent segment to be allocated
} Stack;

typedef struct VM {
  GC gc;
  Namespaces namespaces;
  InstTable instTable;
  ValueTypeTable valueTypeTable;
  RefRegistry refs;
  Stack stack;
  Frame_t current;
} VM;

// frames

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

uint8_t readInstruction(Frame_t frame);
uint16_t readIndex(Frame_t frame);
void setPc(Frame_t frame, uint16_t newPc);
Value getConst(Frame_t frame, uint16_t constantIndex);
Value getLocal(Frame_t frame, uint16_t localIndex);
void setLocal(Frame_t frame, uint16_t localIndex, Value value);
Value* getLocalRef(Frame_t frame, uint16_t localIndex);
uint16_t numLocals(Frame_t frame);
uint64_t numOperands(Frame_t frame);
Value* getOperandRef(Frame_t frame, uint64_t opIndex);
void pushOperand(Frame_t frame, Value value);
Value popOperand(Frame_t frame);
Value getFnRef(Frame_t frame);
void setFnRef(VM *vm, Frame_t frame, Value value);

bool hasResult(Frame_t frame);
bool hasParent(Frame_t frame);
Frame_t getParent(Frame_t frame);
void setResult(Frame_t frame, Value result);
Value getResult(Frame_t frame);

typedef struct ExceptionHandler {
  uint16_t jumpAddress;
  uint16_t localIndex;
} ExceptionHandler;

bool hasHandler(Frame_t frame);
ExceptionHandler getHandler(Frame_t frame);
void setHandler(Frame_t frame, ExceptionHandler handler);
void clearHandler(Frame_t frame);

bool hasFnName(Frame_t frame);
Text getFnName(Frame_t frame);

bool hasSourceTable(Frame_t frame);
bool getLineNumber(Frame_t frame, uint64_t *lineNumber);
bool getFileName(Frame_t frame, Text *fileName);

bool hasException(Frame_t frame);
void setException(Frame_t frame, VMException e);
VMException getException(Frame_t frame);

Frame_t pushFrame(VM *vm, Value newFn);
Frame_t replaceFrame(VM *vm, Value newFn);
Frame_t popFrame(VM *vm);

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

void collect(VM *vm);

#define R_OOM 1

/*
 * Allocates, returns R_OOM if allocation fails. Doesn't attempt collection.
 */
int _alloc(GC *gc, uint64_t length, void **ptr, uint64_t *offset) {

  if (length < 8) {
    explode("oops, allocation size must be >= 8 bytes%" PRIu64, length);
  }

  if ((length & (uint64_t)0x7) != 0) {
    explode("oops, allocation was not 8-byte padded %" PRIu64, length);
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
void* alloc(VM *vm, uint64_t length, Value *value) {

  void *ptr = NULL;
  uint64_t offset = 0;

  int success = _alloc(&vm->gc, length, &ptr, &offset);

  if (success == R_OOM) {
    collect(vm);

    success = _alloc(&vm->gc, length, &ptr, &offset);

    if (success == R_OOM) {
      explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

  value->value = offset;

  return ptr;
}

uint64_t padAllocSize(uint64_t length) {
  /*
   * each object must be at least the size of a pointer, so we can replace it with a
   * forwarding pointer during gc
   */
  if (length < 8) {
    return 8;
  }
  else {
    uint16_t rem = length % 8;
    if (rem != 0) {
      return length + (8 - rem);
    }
    else {
      return length;
    }
  }
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
    uint64_t size = ((ObjectHeader*)ptr)->size;

    void *newPtr = NULL;
    uint64_t offset = 0;

    if (_alloc(gc, size, &newPtr, &offset) == R_OOM) {
      explode("out of memory, cannot allocate %" PRIu64 " bytes mid-gc", size);
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

void collect(VM *vm) {

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
  Frame_t current = vm->current;
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

  uint64_t nameSize = (fnConst->name.length + 1) * sizeof(wchar_t);
  uint64_t constantsSize = fnConst->numConstants * sizeof(Value);
  uint64_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
  uint64_t sourceFileNameSize = (fnConst->code.sourceTable.fileName.length + 1) * sizeof(wchar_t);
  uint64_t lineNumbersSize = fnConst->code.sourceTable.numLineNumbers * sizeof(LineNumber);

  uint64_t fnSize = padAllocSize(sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize + lineNumbersSize);

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

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t strSize = padAllocSize(sizeof(String) + textSize);

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

  uint64_t textSize = (symConst.length + 1) * sizeof(wchar_t);
  uint64_t size = padAllocSize(sizeof(Symbol) + textSize);

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

  uint64_t textSize = (kwConst.length + 1) * sizeof(wchar_t);
  uint64_t size = padAllocSize(sizeof(Keyword) + textSize);

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

  uint64_t size = padAllocSize(sizeof(Cons));

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

Value allocateCons(VM *vm, Value value, Value next) {

  if (next.type != VT_NIL && next.type != VT_LIST) {
    explode("a Cons next must be nil or a list: %s", getValueTypeName(vm, next.type));
  }

  Cons *cons = NULL;

  uint64_t size = padAllocSize(sizeof(Cons));

  Value ptr;
  ptr.type = VT_LIST;
  cons = alloc(vm, size, &ptr);

  consInitContents(cons);
  cons->header.type = VT_LIST;
  cons->header.size = size;
  cons->value = value;
  cons->next = next;

  return ptr;
}

/*
 * Instruction Definitions
 */

// (8), typeIndex (16) | (-> value)
RetVal tryLoadConstEval(VM *vm, Frame_t frame, Error *error) {
  uint16_t constantIndex = readIndex(frame);
  Value constant = getConst(frame, constantIndex);
  pushOperand(frame, constant);
  return R_SUCCESS;
}

// (8), typeIndex (16) | (-> value)
RetVal tryLoadLocalEval(VM *vm, Frame_t frame, Error *error) {
  uint16_t localIndex = readIndex(frame);
  Value v = getLocal(frame, localIndex);
  pushOperand(frame, v);
  return R_SUCCESS;
}

// (8), typeIndex  (16) | (objectref ->)
RetVal tryStoreLocalEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  uint16_t localIndex = readIndex(frame);
  Value v = popOperand(frame);
  setLocal(frame, localIndex, v);

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
    default:
      // fail: not all values are invocable
      throwRuntimeError(error, "cannot invoke this value type as a function: %s",
          getValueTypeName(vm, invocable->fnRef.type));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryPreprocessArguments(VM *vm, Frame_t parent, uint16_t numArgs, bool usesVarArgs, Error *error) {

  RetVal ret;

  Value numArgsSupplied = popOperand(parent);

  if (numArgsSupplied.type != VT_UINT) {
    explode("first op stack value must be number of arguments supplied: %s", getValueTypeName(vm, numArgsSupplied.type));
  }

  if (!usesVarArgs) {
    if (numArgsSupplied.value != numArgs) {
      throwRuntimeError(error, "required arguments not supplied, expected %u but got %" PRIu64, numArgs,
          numArgsSupplied.value);
    }
  }
  else {

    uint16_t numVarArgs;
    if (numArgsSupplied.value > numArgs) {
      numVarArgs = (numArgsSupplied.value - numArgs) + 1;
    }
    else if (numArgsSupplied.value == numArgs) {
      numVarArgs = 1;
    }
    else if (numArgsSupplied.value == numArgs - 1) {
      numVarArgs = 0;
    }
    else {
      throwRuntimeError(error, "required arguments not supplied, expected %u or more arguments but got %" PRIu64,
                        numArgs - 1, numArgsSupplied.value);
    }

    // TODO: stop using the operand stack as a general purpose stack, since it can overflow this way

    // push empty varargs sequence
    pushOperand(parent, nil());

    // read the extra args into that sequence, push it back on the stack
    for (uint16_t i = 0; i < numVarArgs; i++) {

      // may gc, so has to happen before we pop anything off the stack
      Value seq = allocateCons(vm, nil(), nil());

      // gc possibility over, so pop sequence and arg from the stack and set them on cons
      Cons *cons = deref(&vm->gc, seq);

      cons->next = popOperand(parent);
      cons->value = popOperand(parent);

      // put the new sequence back on the stack
      pushOperand(parent, seq);
    }
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryInvokePopulateLocals(VM *vm, Frame_t parent, Frame_t child, Invocable invocable, Error *error) {
  RetVal ret;

  throws(tryPreprocessArguments(vm, parent, invocable.fn->numArgs, invocable.fn->usesVarArgs, error));

  for (uint16_t i = 0; i < invocable.fn->numArgs; i++) {
    Value arg = popOperand(parent);

    uint16_t idx = invocable.fn->numArgs - (1 + i);
    setLocal(child, idx, arg);
  }

  if (invocable.fn->numCaptures > 0) {

    if (!invocable.hasClosure) {
      explode("cannot invoke this fn without a closure, it captures variables: %u",
          invocable.fn->numCaptures);
    }
    if (invocable.closure->numCaptures < invocable.fn->numCaptures) {
      explode("closure does not have enough captured variables: %u",
          invocable.closure->numCaptures);
    }

    uint16_t nextLocalIdx = invocable.fn->numArgs;
    for (uint16_t i=0; i<invocable.fn->numCaptures; i++) {
      setLocal(child, nextLocalIdx, closureCaptures(invocable.closure)[i]);
      nextLocalIdx = nextLocalIdx + 1;
    }
  }

  if (invocable.fn->hasName) {
    setLocal(child, invocable.fn->bindingSlotIndex, invocable.ref);
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryInvokeCFn(VM *vm, Frame_t frame, Value cFn, Error *error) {
  RetVal ret;

  CFn *fn = deref(&vm->gc, cFn);
  throws(tryPreprocessArguments(vm, frame, fn->numArgs, fn->usesVarArgs, error));
  throws(fn->ptr(vm, frame, error));

  return R_SUCCESS;
  failure:
  return ret;
}

// (8)              | (objectref, args... -> ...)
RetVal tryInvokeDynEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  // for cleanup on failure
  bool pushed = false;

  Value pop = popOperand(frame);

  if (pop.type == VT_CFN) {
    throws(tryInvokeCFn(vm, frame, pop, error));
  }
  else {
    Invocable invocable;
    throws(tryPopInvocable(vm, pop, &invocable, error));

    frame = pushFrame(vm, invocable.fnRef);
    pushed = true;

    Frame_t parent = getParent(frame);

    throws(tryInvokePopulateLocals(vm, parent, frame, invocable, error));
  }

  return R_SUCCESS;

  failure:
    if (pushed) {
      popFrame(vm);
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
RetVal tryInvokeDynTailEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value pop = popOperand(frame);

  if (pop.type == VT_CFN) {
    throws(tryInvokeCFn(vm, frame, pop, error));
  }
  else {
    Invocable invocable;
    throws(tryPopInvocable(vm, pop, &invocable, error));

    replaceFrame(vm, invocable.fnRef);
    throws(tryInvokePopulateLocals(vm, frame, frame, invocable, error));
  }

  return R_SUCCESS;
  failure:
    return ret;

}

// (8)              | (objectref ->)
RetVal tryRetEval(VM *vm, Frame_t frame, Error *error) {
  Value v = popOperand(frame);
  setResult(frame, v);
  return R_SUCCESS;
}

// (8)              | (a, b -> 0 | 1)
RetVal tryCmpEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value a = popOperand(frame);
  Value b = popOperand(frame);

  Value c;
  c.type = VT_BOOL;
  c.value = false;

  if (a.type == b.type) {
    switch (a.type) {
      case VT_NIL:
      case VT_UINT:
      case VT_BOOL:
      case VT_FN:
      case VT_CLOSURE:
      case VT_CFN:
      case VT_LIST:
      case VT_STR:
      case VT_SYMBOL:
      case VT_KEYWORD: {
        c.value = a.value == b.value;
        break;
      }
      default:
        explode("Unhandled");
    }
  }

  pushOperand(frame, c);

  return R_SUCCESS;
}

// (8), offset (16) | (->)
RetVal tryJmpEval(VM *vm, Frame_t frame, Error *error) {
  uint16_t newPc = readIndex(frame);
  setPc(frame, newPc);
  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfEval(VM *vm, Frame_t frame, Error *error) {
  Value test = popOperand(frame);

  bool truthy = isTruthy(vm, test);

  uint16_t newPc = readIndex(frame);
  if (truthy) {
    setPc(frame, newPc);
  }

  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfNotEval(VM *vm, Frame_t frame, Error *error) {
  Value test = popOperand(frame);

  bool truthy = isTruthy(vm, test);

  uint16_t newPc = readIndex(frame);
  if (!truthy) {
    setPc(frame, newPc);
  }

  return R_SUCCESS;
}

// (8)              | (a, b -> c)
RetVal tryAddEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (a.type != VT_UINT) {
    throwRuntimeError(error, "can only add integers: %s", getValueTypeName(vm, a.type));
  }
  if (b.type != VT_UINT) {
    throwRuntimeError(error, "can only add integers: %s", getValueTypeName(vm, b.type));
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value + b.value;

  pushOperand(frame, c);

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal trySubEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (a.type != VT_UINT) {
    throwRuntimeError(error, "can only subtract integers: %s", getValueTypeName(vm, a.type));
  }
  if (b.type != VT_UINT) {
    throwRuntimeError(error, "can only subtract integers: %s", getValueTypeName(vm, b.type));
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value - b.value;

  pushOperand(frame, c);

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16)  | (value ->)
RetVal tryDefVarEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);
  uint16_t constantIndex = readIndex(frame);
  Value varName = getConst(frame, constantIndex);

  String *str = deref(&vm->gc, varName);

  throws(tryDefVar(&vm->namespaces, stringValue(str), str->length, value, error));

  // define always returns nil
  Value result = nil();
  pushOperand(frame, result);

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset 16  | (-> value)
RetVal tryLoadVarEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex = readIndex(frame);
  Value varName = getConst(frame, constantIndex);

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
    pushOperand(frame, var->value);
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
RetVal tryLoadClosureEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex = readIndex(frame);
  Value fnValue = getConst(frame, constantIndex);

  if (fnValue.type != VT_FN) {
    throwRuntimeError(error, "cannot create a closure from this value type: %s",
        getValueTypeName(vm, fnValue.type));
  }

  Fn *fn = deref(&vm->gc, fnValue);

  Closure *closure = NULL;

  uint64_t capturesSize = fn->numCaptures * sizeof(Value);
  uint64_t clSize = padAllocSize(sizeof(Closure) + capturesSize);

  Value closureValue;
  closureValue.type = VT_CLOSURE;

  closure = alloc(vm, clSize, &closureValue);

  closureInitContents(closure);
  closure->header.type = VT_CLOSURE;
  closure->header.size = clSize;
  closure->fn = fnValue;
  closure->numCaptures = fn->numCaptures;

  closure->capturesOffset = sizeof(Closure);

  // pop captures in reverse order, same as arguments
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    Value capture = popOperand(frame);
    uint16_t idx = closure->numCaptures - (1 + i);
    closureCaptures(closure)[idx] = capture;
  }

  pushOperand(frame, closureValue);

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)        | (a, b -> b, a)
RetVal trySwapEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value a = popOperand(frame);
  Value b = popOperand(frame);

  pushOperand(frame, a);
  pushOperand(frame, b);

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)        | (jumpAddr, handler ->)
RetVal trySetHandlerEval(VM *vm, Frame_t frame, Error *error) {
  ExceptionHandler handler;

  handler.jumpAddress = readIndex(frame);
  handler.localIndex = readIndex(frame);

  setHandler(frame, handler);

  return R_SUCCESS;
}

// (8)        | (->)
RetVal tryClearHandlerEval(VM *vm, Frame_t frame, Error *error) {
  clearHandler(frame);
  return R_SUCCESS;
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
RetVal tryConsEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  // gc may occur, so allocate the cons first
  Value result = allocateCons(vm, nil(), nil());

  Value seq = popOperand(frame);
  Value x = popOperand(frame);

  if (seq.type != VT_NIL && seq.type != VT_LIST) {
    throwRuntimeError(error, "cannot cons onto a value of type %s", getValueTypeName(vm, seq.type));
  }

  Cons *cons = deref(&vm->gc, result);
  cons->value = x;
  cons->next = seq;

  pushOperand(frame, result);

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> x)
RetVal tryFirstEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value seq = popOperand(frame);

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

  pushOperand(frame, result);

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> seq)
RetVal tryRestEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value seq = popOperand(frame);

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

  pushOperand(frame, result);

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (name -> nil)
RetVal trySetMacroEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value strValue = popOperand(frame);

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

  pushOperand(frame, nil());

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (name -> bool)
RetVal tryGetMacroEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value strValue = popOperand(frame);

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

  pushOperand(frame, result);

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (name -> bool)
RetVal tryGCEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  collect(vm);

  pushOperand(frame, nil());

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (value -> value)
RetVal tryGetTypeEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);

  Value typeId;
  typeId.type = VT_UINT;
  typeId.value = value.type;

  pushOperand(frame, typeId);

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVMPrn(VM *vm, Value result, Pool_t pool, Expr *expr, Error *error);

#define ONE_KB 1024

// (8),             | (value -> value)
RetVal tryPrnEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_KB, error));

  Value value = popOperand(frame);

  Expr expr;
  throws(tryVMPrn(vm, value, pool, &expr, error));
  throws(tryExprPrn(pool, &expr, error));
  printf("\n");

  pushOperand(frame, nil());

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
                        .tryEquals = &tryEqualsNil,},
      [VT_UINT]      = {.name = "uint",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnInt,
                        .tryEquals = &tryEqualsUInt},
      [VT_BOOL]      = {.name = "bool",
                        .isHeapObject = false,
                        .isTruthy = &isTruthyBool,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnBool,
                        .tryEquals = &tryEqualsBool},
      [VT_FN]        = {.name = "fn",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenFn,
                        .tryPrn = &tryPrnFn,
                        .tryEquals = &tryEqualsFn},
      [VT_STR]       = {.name = "str",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnStr,
                        .tryEquals = &tryEqualsStr},
      [VT_SYMBOL]    = {.name = "symbol",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnSymbol,
                        .tryEquals = &tryEqualsSymbol},
      [VT_KEYWORD]   = {.name = "keyword",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnKeyword,
                        .tryEquals = &tryEqualsKeyword},
      [VT_LIST]      = {.name = "list",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenList,
                        .tryPrn = &tryPrnList,
                        .tryEquals = &tryEqualsList},
      [VT_CLOSURE]   = {.name = "closure",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenClosure,
                        .tryPrn = &tryPrnClosure,
                        .tryEquals = &tryEqualsClosure},
      [VT_CFN]       = {.name = "cfn",
                        .isHeapObject = true,
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnCFn,
                        .tryEquals = &tryEqualsCFn}
  };
  memcpy(table.valueTypes, valueTypes, sizeof(valueTypes));
  table.numValueTypes = sizeof(valueTypes) / sizeof(valueTypes[0]);

  return table;
}

void printEvalError(Frame_t frame, Error *error) {

  printf("unhandled error: %ls", error->message);

  Frame_t current = frame;
  while (true) {

    wchar_t *fnName;
    if (hasFnName(current)) {
      Text text = getFnName(current);
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

void exFrameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

RetVal tryExceptionMake(Frame_t frame, Pool_t pool, VMException *exception, Error *error) {
  RetVal ret;

  Error reference = *error;

  wchar_t msg[ERROR_MSG_LENGTH];
  exceptionInitContents(exception);

  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", reference.message);
  throws(tryTextMake(pool, msg, &exception->message, wcslen(msg), error));

  uint64_t numFrames = 0;
  {
    Frame_t current = frame;
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

  Frame_t current = frame;
  for (uint64_t i=1; i<numFrames; i++) {

    VMExceptionFrame *f = &exception->frames.elements[i];
    exFrameInitContents(f);

    if (hasFnName(current)) {
      Text text = getFnName(current);
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

RetVal tryFrameEval(VM *vm, Pool_t outputPool, Error *error) {
  RetVal ret;

  uint8_t inst;
  TryEval tryEval;

  while (true) {

    if (hasResult(vm->current)) {
      if (!hasParent(vm->current)) {
        break;
      }
      else {
        Value result = getResult(vm->current);
        Frame_t parent = getParent(vm->current);
        pushOperand(parent, result);

        popFrame(vm);
      }
    }

    inst = readInstruction(vm->current);
    tryEval = vm->instTable.instructions[inst].tryEval;

    if (tryEval == NULL) {
      explode("instruction unimplemented: %s (%u)", getInstName(&vm->instTable, inst), inst);
    }

    ret = tryEval(vm, vm->current, error);

    if (ret != R_SUCCESS) {
      VMException ex;
      // TODO: exceptions should go on the heap
      throws(tryExceptionMake(vm->current, outputPool, &ex, error));
      setException(vm->current, ex);
      break;
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

/*
 * Call Stack Implementation
 */

void stackSegmentInitContents(StackSegment *segment) {
  segment->data = NULL;
  segment->dataEnd = NULL;
  segment->allocPtr = NULL;
  segment->prev = NULL;
  segment->next = NULL;
}

void stackInitContents(Stack *pool, uint64_t segmentSize) {
  pool->segmentSize = segmentSize;
  pool->root = NULL;
  pool->current = NULL;
}

StackSegment* makeSegment(uint64_t segmentSize) {

  StackSegment *segment = malloc(sizeof(StackSegment));
  if (segment == NULL) {
    explode("failed to allocate stack segment memory")
  }

  stackSegmentInitContents(segment);

  segment->data = malloc(segmentSize);
  if (segment->data == NULL) {
    explode("failed to allocate stack segment memory block")
  }

  segment->allocPtr = segment->data;
  segment->dataEnd = segment->data + segmentSize;
  segment->prev = NULL;
  segment->next = NULL;

  return segment;
}

void freeSegment(StackSegment *segment) {
  if (segment != NULL) {
    free(segment->data);
    stackSegmentInitContents(segment);
    free(segment);
  }
}

void* stackAllocate(Stack *stack, size_t size, char *description) {

  if (size == 0) {
    return NULL;
  }

  if (size > stack->segmentSize) {
    explode("cannot allocate a size bigger than %zu: %s", size, description);
  }

  if (stack->root == NULL) { // create root segment if missing
    StackSegment *segment = makeSegment(stack->segmentSize);
    stack->root = segment;
    stack->current = segment;
  }
  else if (stack->current->allocPtr + size >= stack->current->dataEnd) { // add new segment
    StackSegment *segment = makeSegment(stack->segmentSize);
    segment->prev = stack->current;
    stack->current->next = segment;
    stack->current = segment;
  }

  void* ret = stack->current->allocPtr;
  stack->current->allocPtr += size;

  return ret;
}

/*
 * unwinds the stack allocation to immediately before the specified address
 */
void stackFree(Stack *stack, void *ptr) {

  if (stack->root == NULL) {
    explode("stack is not initialized");
  }

  { // bounds check
    void *start = stack->root->data;
    void *end = stack->current->dataEnd;
    if (ptr < start || ptr >= end) {
      explode("cannot free data, it is outside the stack address range [%" PRIu64 " - %" PRIu64 "]: %" PRIu64,
              (uint64_t) start, (uint64_t) end, (uint64_t) ptr);
    }
  }

  // free intermediate segments
  while (ptr < stack->current->data) {
    StackSegment *freeMe = stack->current;
    stack->current = stack->current->prev;
    freeSegment(freeMe);
  }
  stack->current->next = NULL;

  // update alloc for current segment
  stack->current->allocPtr = ptr;
}

uint64_t stackSize(Stack *stack) {

  uint64_t result = 0;

  StackSegment *cursor = stack->root;
  while (cursor != NULL) {
    result += stack->segmentSize;
    cursor = cursor->next;
  }

  return result;
}

void stackClear(Stack *stack) {

  StackSegment *cursor = stack->root;
  while (cursor != NULL) {
    free(cursor->data);
    cursor = cursor->next;
  }

  stack->root = NULL;
  stack->current = NULL;
}

void stackFreeContents(Stack *stack) {
  if (stack != NULL) {
    stackClear(stack);
  }
}

typedef struct Frame Frame;

typedef struct Frame {
  Frame *parent;

  Value fnRef;
  Fn *fn;

  Value *locals;

  uint64_t opStackMaxDepth;
  uint64_t opStackUsedDepth;
  Value *opStack;

  Value result;
  bool resultAvailable;
  uint16_t pc;

  ExceptionHandler handler;
  bool handlerSet;

  VMException exception;
  bool exceptionSet;
} Frame;

void handlerInitContents(ExceptionHandler *h) {
  h->localIndex = 0;
  h->jumpAddress = 0;
}

void frameInitContents(Frame *frame) {
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

  frame->opStackUsedDepth = 0;
  frame->opStackMaxDepth = 0;
  frame->opStack = NULL;
}

uint8_t readInstruction(Frame *frame) {

  if (frame->pc >= frame->fn->codeLength) {
    explode("cannot read next instruction, no instructions left");
  }

  uint8_t inst = fnCode(frame->fn)[frame->pc];
  frame->pc += 1;
  return inst;
}

uint16_t readIndex(Frame *frame) {

  if (frame->pc + 1 >= frame->fn->codeLength) {
    explode("cannot read next instruction, no instructions left");
  }

  uint8_t *code = fnCode(frame->fn);
  uint16_t pc = frame->pc;
  uint16_t index = (code[pc] << 8) | code[pc + 1];
  frame->pc += 2;

  return index;
}

void setPc(Frame *frame, uint16_t newPc) {
  if (newPc >= frame->fn->codeLength) {
    explode("no such instruction: %u", newPc);
  }
  frame->pc = newPc;
}

Value getConst(Frame *frame, uint16_t constantIndex) {
  if (constantIndex >= frame->fn->numConstants) {
    explode("no such constant: %u", constantIndex);
  }
  return fnConstants(frame->fn)[constantIndex];
}

Value getLocal(Frame *frame, uint16_t localIndex) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  return frame->locals[localIndex];
}

Value* getLocalRef(Frame *frame, uint16_t localIndex) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  return &frame->locals[localIndex];
}

void setLocal(Frame *frame, uint16_t localIndex, Value value) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  frame->locals[localIndex] = value;
}

uint16_t numLocals(Frame *frame) {
  return frame->fn->numLocals;
}

uint64_t numOperands(Frame *frame) {
  return frame->opStackUsedDepth;
}

Value* getOperandRef(Frame *frame, uint64_t opIndex) {
  if (opIndex >= frame->opStackUsedDepth) {
    explode("no such operand: %" PRIu64, opIndex);
  }
  return &frame->opStack[opIndex];
}

void pushOperand(Frame *frame, Value value) {
  if (frame->opStackMaxDepth == frame->opStackUsedDepth + 1) {
    explode("cannot allocate op stack greater than max %" PRIu64, frame->opStackMaxDepth);
  }
  frame->opStack[frame->opStackUsedDepth] = value;
  frame->opStackUsedDepth++;
}

Value popOperand(Frame *frame) {
  if (frame->opStackUsedDepth == 0) {
    explode("cannot pop from empty op stack")
  }
  frame->opStackUsedDepth--;
  return frame->opStack[frame->opStackUsedDepth];
}

Value getFnRef(Frame *frame) {
  return frame->fnRef;
}

void setFnRef(VM *vm, Frame *frame, Value value) {
  frame->fnRef = value;
  frame->fn = deref(&vm->gc, value);
}

bool hasResult(Frame *frame) {
  return frame->resultAvailable;
}

bool hasParent(Frame *frame) {
  return frame->parent != NULL;
}

Frame_t getParent(Frame *frame) {
  if (frame->parent == NULL) {
    explode("no parent available");
  }
  return frame->parent;
}

void setResult(Frame *frame, Value result) {
  if (frame->resultAvailable) {
    explode("result already set");
  }
  frame->result = result;
  frame->resultAvailable = true;
}

Value getResult(Frame *frame) {
  if (!frame->resultAvailable) {
    explode("result not set");
  }
  return frame->result;
}

bool hasHandler(Frame *frame) {
  return frame->handlerSet;
}

ExceptionHandler getHandler(Frame_t frame) {
  if (!frame->handlerSet) {
    explode("handler not set");
  }
  return frame->handler;
}

void setHandler(Frame_t frame, ExceptionHandler handler) {
  frame->handler = handler;
  frame->handlerSet = true;
}

void clearHandler(Frame_t frame) {
  handlerInitContents(&frame->handler);
  frame->handlerSet = false;
}

bool hasFnName(Frame *frame) {
  return frame->fn->hasName;
}

Text getFnName(Frame_t frame) {
  if (!frame->fn->hasName) {
    explode("no fn name found");
  }
  Text name;
  textInitContents(&name);
  name.value = fnName(frame->fn);
  name.length = frame->fn->nameLength;
  return name;
}

bool hasSourceTable(Frame *frame) {
  return frame->fn->hasSourceTable;
}

bool getLineNumber(Frame *frame, uint64_t *lineNumber) {
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

bool getFileName(Frame_t frame, Text *fileName) {
  if (frame->fn->hasSourceTable) {
    fileName->value = fnSourceFileName(frame->fn);
    fileName->length = frame->fn->sourceFileNameLength;
    return true;
  }
  return false;
}

bool hasException(Frame_t frame) {
  return frame->exceptionSet;
}

void setException(Frame_t frame, VMException e) {
  frame->exception = e;
  frame->exceptionSet = true;
}

VMException getException(Frame_t frame) {
  if (!frame->exceptionSet) {
    explode("handler not set");
  }
  return frame->exception;
}

Frame_t pushFrame(VM *vm, Value newFn) {

  Fn *fn = deref(&vm->gc, newFn);

  Stack *stack = &vm->stack;
  Frame *parent = vm->current;

  Frame *frame = stackAllocate(stack, sizeof(Frame), "ExecFrame");
  frameInitContents(frame);

  frame->parent = parent;
  frame->fnRef = newFn;
  frame->fn = fn;

  frame->locals = stackAllocate(stack, sizeof(Value) * frame->fn->numLocals, "locals");

  frame->opStackMaxDepth = frame->fn->maxOperandStackSize;
  frame->opStackUsedDepth = 0;
  frame->opStack = stackAllocate(stack, sizeof(Value) * frame->opStackMaxDepth, "opStack");

  vm->current = frame;

  return frame;
}

Frame* popFrame(VM *vm) {

  if (vm->current == NULL) {
    explode("no frames on stack");
  }

  Frame *popped = vm->current;
  vm->current = vm->current->parent;
  stackFree(&vm->stack, popped);

  return vm->current;
}

Frame* replaceFrame(VM *vm, Value newFn) {
  Fn *fn = deref(&vm->gc, newFn);

  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  if (fn->numLocals > frame->fn->numLocals) {
    frame->locals = stackAllocate(stack, sizeof(Value) * fn->numLocals, "locals");
  }

  if (fn->maxOperandStackSize > frame->opStackMaxDepth) {
    frame->opStackMaxDepth = fn->maxOperandStackSize;
    frame->opStack = stackAllocate(stack, sizeof(Value) * frame->opStackMaxDepth, "opStack");
  }

  frame->fnRef = newFn;
  frame->fn = fn;
  frame->result = nil();
  frame->resultAvailable = false;
  frame->pc = 0;

  return frame;
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

  Frame_t frame = pushFrame(vm, fnRef);
  pushed = true;

  throws(tryFrameEval(vm, outputPool, error));

  if (frame->exceptionSet) {
    *exceptionThrown = true;
    *exception = frame->exception;
  }
  else {
    *result = frame->result;
  }

  popFrame(vm);

  return R_SUCCESS;

  failure:
    if (pushed) {
      popFrame(vm);
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

RetVal tryStringMakeBlank(VM *vm, Frame_t frame, uint64_t length, Value *value, Error *error) {

  String *str = NULL;

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t strSize = padAllocSize(sizeof(String) + textSize);

  value->type = VT_STR;
  str = alloc(vm, strSize, value);

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
RetVal tryStrJoinBuiltin(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value strings = popOperand(frame);
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
  pushOperand(frame, strings);

  Value resultRef = nil();
  throws(tryStringMakeBlank(vm, frame, totalLength, &resultRef, error));
  String *result = deref(&vm->gc, resultRef);

  // get the list back again after allocation
  strings = popOperand(frame);

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

  pushOperand(frame, resultRef);

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPrStrBuiltinConf(VM *vm, Frame_t frame, bool readable, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);

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

  pushOperand(frame, resultRef);

  ret = R_SUCCESS;
  goto done;

  failure:
  goto done;

  done:
    // clean up off-heap memory
    poolFree(pool);
    return ret;
}

RetVal tryPrStrBuiltin(VM *vm, Frame_t frame, Error *error) {
  return tryPrStrBuiltinConf(vm, frame, true, error);
}

RetVal tryPrintStrBuiltin(VM *vm, Frame_t frame, Error *error) {
  return tryPrStrBuiltinConf(vm, frame, false, error);
}

RetVal trySymbolMakeBlank(VM *vm, Frame_t frame, uint64_t length, Value *result, Error *error) {
  Symbol *sym = NULL;

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t size = padAllocSize(sizeof(Symbol) + textSize);

  result->type = VT_SYMBOL;
  sym = alloc(vm, size, result);

  symbolInitContents(sym);
  sym->header.type = VT_SYMBOL;
  sym->header.size = size;
  sym->length = length;

  sym->valueOffset = sizeof(Symbol);
  symbolValue(sym)[sym->length] = L'\0';

  return R_SUCCESS;
}

RetVal trySymbolBuiltin(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);

  ASSERT_STR(value);

  uint64_t length = 0;
  {
    String *string = deref(&vm->gc, value);
    length = string->length;
  }

  // keep the string safely on the op stack while we allocate
  pushOperand(frame, value);

  Value result;
  throws(trySymbolMakeBlank(vm, frame, length, &result, error));

  // pop string back off now we're done allocating
  value = popOperand(frame);

  // actually copy string into symbol
  String *string = deref(&vm->gc, value);
  Symbol *sym = deref(&vm->gc, result);
  memcpy(symbolValue(sym), stringValue(string), sym->length * sizeof(wchar_t));

  pushOperand(frame, result);

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryKeywordMakeBlank(VM *vm, Frame_t frame, uint64_t length, Value *result, Error *error) {
  Keyword *kw = NULL;

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t size = padAllocSize(sizeof(Keyword) + textSize);

  result->type = VT_KEYWORD;
  kw = alloc(vm, size, result);

  keywordInitContents(kw);
  kw->header.type = VT_KEYWORD;
  kw->header.size = size;
  kw->length = length;

  kw->valueOffset = sizeof(Symbol);
  keywordValue(kw)[kw->length] = L'\0';

  return R_SUCCESS;
}

RetVal tryKeywordBuiltin(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);

  ASSERT_STR(value);

  uint64_t length = 0;
  {
    String *string = deref(&vm->gc, value);
    length = string->length;
  }

  // keep the string safely on the op stack while we allocate
  pushOperand(frame, value);

  Value result;
  throws(tryKeywordMakeBlank(vm, frame, length, &result, error));

  // pop string back off now we're done allocating
  value = popOperand(frame);

  // actually copy string into symbol
  String *string = deref(&vm->gc, value);
  Keyword *kw = deref(&vm->gc, result);
  memcpy(keywordValue(kw), stringValue(string), kw->length * sizeof(wchar_t));

  pushOperand(frame, result);

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

Value makeCFn(VM *vm, const wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr) {
  CFn *fn = NULL;

  uint64_t nameLength = wcslen(name);
  uint64_t nameSize = (nameLength + 1) * sizeof(wchar_t);
  uint64_t fnSize = padAllocSize(sizeof(CFn) + nameSize);

  uint64_t offset = 0;

  if (_alloc(&vm->gc, fnSize, (void*)&fn, &offset) == R_OOM) {
    explode("out of memory, failed to allocate CFn: %ls", name);
  }

  Value value;
  value.type = VT_CFN;
  value.value = offset;

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

  return value;
}

void defineCFn(VM *vm, wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr) {

  size_t nameLength = wcslen(name);
  Value value = makeCFn(vm, name, numArgs, varArgs, ptr);

  Error error;
  errorInitContents(&error);
  if (tryDefVar(&vm->namespaces, name, nameLength, value, &error) != R_SUCCESS) {
    explode("failed to define a var");
  }
}


void initCFns(VM *vm) {

  defineCFn(vm, L"cons", 2, false, tryConsEval);
  defineCFn(vm, L"first", 1, false, tryFirstEval);
  defineCFn(vm, L"rest", 1, false, tryRestEval);
  defineCFn(vm, L"set-macro", 1, false, trySetMacroEval);
  defineCFn(vm, L"get-macro", 1, false, tryGetMacroEval);
  defineCFn(vm, L"gc", 0, false, tryGCEval);
  defineCFn(vm, L"get-type", 1, false, tryGetTypeEval);
  defineCFn(vm, L"prn", 1, false, tryPrnEval);
  defineCFn(vm, L"+", 2, false, tryAddEval);
  defineCFn(vm, L"-", 2, false, trySubEval);
  defineCFn(vm, L"eq", 2, false, tryCmpEval);
  defineCFn(vm, L"join", 1, false, tryStrJoinBuiltin);
  defineCFn(vm, L"pr-str", 1, false, tryPrStrBuiltin);
  defineCFn(vm, L"print-str", 1, false, tryPrintStrBuiltin);
  defineCFn(vm, L"symbol", 1, false, trySymbolBuiltin);
  defineCFn(vm, L"keyword", 1, false, tryKeywordBuiltin);
}

RetVal tryVMInitContents(VM *vm , Error *error) {
  RetVal ret;

  vm->instTable = instTableCreate();
  vm->valueTypeTable = valueTypeTableCreate();
  GCCreate(&vm->gc, 1024 * 1000);
  throws(tryNamespacesInitContents(&vm->namespaces, error));
  initCFns(vm);
  refRegistryInitContents(&vm->refs);
  stackInitContents(&vm->stack, 1024 * 1000);
  vm->current = NULL;

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
