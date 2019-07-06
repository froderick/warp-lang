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

typedef uint64_t Value;

#define W_UINT_MASK      0x01u
#define W_UINT_BITS      0x01u
#define W_PTR_MASK       0x03u
#define W_IMMEDIATE_MASK 0x0fu
#define W_BOOLEAN_BITS   0x06u
#define W_SPECIAL_MASK   0xf0u
#define W_NIL_BITS       0x00u
#define W_SPECIAL_BITS   0x0eu // 1110
#define W_NIL_VALUE      0x0eu

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

#define W_GC_FORWARDING_BIT      0x8000000000000000L   /* header contains forwarding pointer */
#define W_BYTEBLOCK_BIT          0x4000000000000000L   /* block contains bytes instead of slots */
#define W_SPECIALBLOCK_BIT       0x2000000000000000L   /* 1st item is a non-value */
#define W_8ALIGN_BIT             0x1000000000000000L   /* data is aligned to 8-byte boundary */
#define W_HEADER_TYPE_BITS       0x0f00000000000000L
#define W_HEADER_SIZE_MASK       0x00ffffffffffffffL

#define W_FN_TYPE      0x0u
#define W_STR_TYPE     0x1u
#define W_SYMBOL_TYPE  0x2u
#define W_KEYWORD_TYPE 0x3u
#define W_LIST_TYPE    0x4u
#define W_CLOSURE_TYPE 0x5u
#define W_CFN_TYPE     0x6u

/*
 * This is the first field inside all heap objects. It must come first so that the GC can
 * scan through the heap, for which it needs to determine object sizes and object types.
 */
typedef uint64_t ObjectHeader;

ObjectHeader makeObjectHeader(uint8_t objectType, uint64_t size) {
  if (objectType > 0xf) {
    explode("too large: %u", objectType);
  }
  if (size > W_HEADER_SIZE_MASK) {
    explode("too large: %" PRIu64, size);
  }
  return (((uint64_t)objectType) << 56u) | size;
}

uint8_t objectHeaderType(ObjectHeader h) {
  return ((h & W_HEADER_TYPE_BITS) >> 56u) & 0xfu;
}

uint64_t objectHeaderSize(ObjectHeader h) {
  return h & W_HEADER_SIZE_MASK;
}

ValueType objectHeaderValueType(ObjectHeader header) {
  switch (objectHeaderType(header)) {
    case W_FN_TYPE: return VT_FN;
    case W_STR_TYPE: return VT_STR;
    case W_SYMBOL_TYPE: return VT_SYMBOL;
    case W_KEYWORD_TYPE: return VT_KEYWORD;
    case W_LIST_TYPE: return VT_LIST;
    case W_CLOSURE_TYPE: return VT_CLOSURE;
    case W_CFN_TYPE: return VT_CFN;
    default:
    explode("unknown type: %u", objectHeaderType(header));
  }
}

ValueType valueType(Value v) {

  if ((v & W_UINT_MASK) == 1) {
    return VT_UINT;
  }

  if ((v & W_PTR_MASK) == 0) {
    ObjectHeader *h = (void*)v;
    return objectHeaderValueType(*h);
  }

  uint8_t imm = v & W_IMMEDIATE_MASK;
  if (imm == W_BOOLEAN_BITS) {
    return VT_BOOL;
  }
  else {
    uint8_t special = v & W_SPECIAL_MASK;
    if (special == W_NIL_BITS) {
      return VT_NIL;
    }
  }

  explode("unknown type: %" PRIu64, v);
}

Value nil() {
  return W_NIL_VALUE;
}

Value wrapBool(bool b) {
  return (((uint8_t)b & 1u) << 4u) | W_BOOLEAN_BITS;
}

bool unwrapBool(Value v) {
  return v >> 4u;
}

Value wrapUint(uint64_t i) {
  return (i << 1u) | W_UINT_BITS;
}

uint64_t unwrapUint(Value v) {
  return v >> 1u;
}

typedef struct Frame *Frame_t;
typedef struct FrameRoot *FrameRoot_t;
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

  Value topLevelValue;
  Value name;
} Symbol;

typedef struct Keyword {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
  uint32_t hash;
} Keyword;

wchar_t* keywordValue(Keyword *x) { return (void*)x + x->valueOffset; }

typedef struct Cons {
  ObjectHeader header;

  Value metadata;
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

typedef void (*RelocateChildren)(VM_t vm, void *obj);
typedef RetVal (*TryPrn)(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error);
typedef RetVal (*TryHashCode)(VM_t vm, Value value, uint32_t *hash, Error *error);
typedef bool (*Equals)(VM_t vm, Value this, Value that);

typedef struct ValueTypeInfo {
  const char *name;
  bool (*isTruthy)(Value value);
  RelocateChildren relocateChildren;
  TryPrn tryPrn;
  TryHashCode tryHashCode;
  Equals equals;
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

void pushFrameRoot(VM *vm, Value *rootPtr);
void popFrameRoot(VM *vm);
FrameRoot_t frameRoots(VM *vm);
Value* frameRootValue(FrameRoot_t root);
FrameRoot_t frameRootNext(FrameRoot_t root);

/*
 * value type protocols
 */

const char* getValueTypeName(VM *vm, uint8_t type) {
  return vm->valueTypeTable.valueTypes[type].name;
}

bool isTruthy(VM *vm, Value value) {
  ValueType t = valueType(value);
  return vm->valueTypeTable.valueTypes[t].isTruthy(value);
}

void relocateChildren(VM *vm, ValueType type, void *obj) {
  RelocateChildren relocate = vm->valueTypeTable.valueTypes[type].relocateChildren;
  if (relocate != NULL) {
    relocate(vm, obj);
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
int _alloc(GC *gc, uint64_t length, void **ptr) {

  if ((length & W_PTR_MASK) != 0) {
    explode("oops, allocation was not 4-byte padded %" PRIu64, length);
  }

  if (gc->allocPtr + length < gc->currentHeapEnd) {
    *ptr = gc->allocPtr;
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
void* alloc(VM *vm, uint64_t length) {

  void *ptr = NULL;

  int success = _alloc(&vm->gc, length, &ptr);

  if (success == R_OOM) {
    collect(vm);

    success = _alloc(&vm->gc, length, &ptr);

    if (success == R_OOM) {
      explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

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

// keeping this *for now*, it makes things slower but safer
void* deref(GC *gc, Value value) {
  void *ptr = (void*)value;
  if (ptr < gc->currentHeap || ptr >= gc->allocPtr) {
    explode("invalid memory address: %p", ptr);
  }
  return ptr;
}

void relocate(VM *vm, Value *valuePtr) {

  Value value = *valuePtr;

  if ((value & W_PTR_MASK) != 0) {
    // only relocate heap objects
    return;
  }

  ObjectHeader *header = (ObjectHeader*)value;
  if (*header & W_GC_FORWARDING_BIT) {
    *valuePtr = (*header << 1u);
  }
  else {
    uint64_t size = objectHeaderSize(*header);

    void *newPtr = NULL;
    if (_alloc(&vm->gc, size, &newPtr) == R_OOM) {
      explode("out of memory, cannot allocate %" PRIu64 " bytes mid-gc", size);
    }
    memcpy(newPtr, (void*)value, size);

    *valuePtr = (Value)newPtr;
    *header = W_GC_FORWARDING_BIT | ((Value)newPtr >> 1u);
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
    relocate(vm, &refElem->heapObject);
    refElem = refElem->next;
  }

  // relocate var roots
  for (uint64_t i=0; i<vm->namespaces.numNamespaces; i++) {
    Namespace *ns = &vm->namespaces.namespaces[i];
    for (uint64_t j=0; j<ns->localVars.length; j++) {
      Var *var = &ns->localVars.vars[j];
      relocate(vm, &var->value);
    }
  }

  // relocate call stack roots
  Frame_t current = vm->current;
  while (true) {

    // relocate fnRef
    {
      Value oldFnRef = getFnRef(current);
      Value newFnRef = oldFnRef;
      relocate(vm, &newFnRef);
      setFnRef(vm, current, newFnRef);
    }

    uint16_t locals = numLocals(current);
    for (uint16_t i=0; i<locals; i++) {
      Value *val = getLocalRef(current, i);
      relocate(vm, val);
    }

    uint64_t operands = numOperands(current);
    for (uint64_t i=0; i<operands; i++) {
      Value *val = getOperandRef(current, i);
      relocate(vm, val);
    }

    FrameRoot_t root = frameRoots(vm);
    while (root != NULL) {
      Value *valuePtr = frameRootValue(root);
      relocate(vm, valuePtr);
      root = frameRootNext(root);
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
    relocateChildren(vm, objectHeaderValueType(*header), scanptr);
    scanptr += objectHeaderSize(*header);
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

ValueType getRefType(Ref ref) {
  RefElem *elem = (RefElem*)ref;
  return valueType(elem->heapObject);
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

void hydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value *values);

void fnInitContents(Fn *fn) {

  fn->header = 0;

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

Value fnHydrate(VM *vm, FnConstant *fnConst) {

  // cleanup on failure
  Fn *fn = NULL;

  uint64_t nameSize = (fnConst->name.length + 1) * sizeof(wchar_t);
  uint64_t constantsSize = fnConst->numConstants * sizeof(Value);
  uint64_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
  uint64_t sourceFileNameSize = (fnConst->code.sourceTable.fileName.length + 1) * sizeof(wchar_t);
  uint64_t lineNumbersSize = fnConst->code.sourceTable.numLineNumbers * sizeof(LineNumber);

  uint64_t fnSize = padAllocSize(sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize + lineNumbersSize);

  fn = alloc(vm, fnSize);

  fnInitContents(fn);

  fn->header = makeObjectHeader(W_FN_TYPE, fnSize);

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
  hydrateConstants(vm, fn->numConstants, fnConst->constants, fnConstants(fn));

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

  return (Value)fn;
}

void stringInitContents(String *s) {
  s->header = 0;
  s->length = 0;
  s->valueOffset = 0;
  s->hash = 0;
}

Value stringHydrate(VM *vm, wchar_t *text, uint64_t length) {
  String *str = NULL;

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t strSize = padAllocSize(sizeof(String) + textSize);

  str = alloc(vm, strSize);

  stringInitContents(str);

  str->header = makeObjectHeader(W_STR_TYPE, strSize);
  str->length = length;

  str->valueOffset = sizeof(String);
  memcpy(stringValue(str), text, length * sizeof(wchar_t));
  stringValue(str)[length] = L'\0';

  return (Value)str;
}

void symbolInitContents(Symbol *s) {
  s->header = 0;
  s->topLevelValue = nil();
  s->name = nil();
}

Value symbolHydrate(VM *vm, SymbolConstant symConst) {
  Symbol *sym = NULL;

  Value string = stringHydrate(vm, symConst.value, symConst.length);

  uint64_t size = padAllocSize(sizeof(Symbol));
  sym = alloc(vm, size);

  symbolInitContents(sym);
  sym->header = makeObjectHeader(W_SYMBOL_TYPE, size);
  sym->topLevelValue = nil();
  sym->name = string;

  return (Value)sym;
}

void keywordInitContents(Keyword *k) {
  k->header = 0;
  k->length = 0;
  k->valueOffset = 0;
  k->hash = 0;
}

Value keywordHydrate(VM *vm, KeywordConstant kwConst) {
  Keyword *kw = NULL;

  uint64_t textSize = (kwConst.length + 1) * sizeof(wchar_t);
  uint64_t size = padAllocSize(sizeof(Keyword) + textSize);

  kw = alloc(vm, size);

  keywordInitContents(kw);
  kw->header = makeObjectHeader(W_KEYWORD_TYPE, size);
  kw->length = kwConst.length;

  kw->valueOffset = sizeof(Keyword);
  memcpy(keywordValue(kw), kwConst.value, kw->length * sizeof(wchar_t));
  keywordValue(kw)[kw->length] = L'\0';

  return (Value)kw;
}

void consInitContents(Cons *c) {
  c->header = 0;
  c->metadata = nil();
  c->value = nil();
  c->next = nil();
}

Value _allocateCons(VM *vm, Value value, Value next, Value meta) {

  ValueType nextType = valueType(next);
  if (nextType != VT_NIL && nextType != VT_LIST) {
    explode("a Cons next must be nil or a list: %u", nextType);
  }

  Cons *cons = NULL;

  uint64_t size = padAllocSize(sizeof(Cons));
  cons = alloc(vm, size);

  consInitContents(cons);
  cons->header = makeObjectHeader(W_LIST_TYPE, size);
  cons->metadata = meta;
  cons->value = value;
  cons->next = next;

  return (Value)cons;
}

/*
 * alreadyHydratedConstants is a pointer to the array of all values materialized for the current fn or codeunit, so far.
 * it is included so that references to already-hydrated values can be resolved by constant index.
 */
Value listHydrate(VM *vm, Value *alreadyHydratedConstants, ListConstant listConst) {

  // build up meta property list with conses
  Value meta = nil();

  for (uint64_t i=0; i<listConst.meta.numProperties; i++) {
    ConstantMetaProperty *p = &listConst.meta.properties[i];
    meta = _allocateCons(vm, alreadyHydratedConstants[p->valueIndex], meta, nil());
    meta = _allocateCons(vm, alreadyHydratedConstants[p->keyIndex], meta, nil());
  }

  // build up list with conses, each cons gets the same meta
  Value seq = nil();

  for (uint16_t i = 0; i < listConst.length; i++) {

    uint16_t listConstEnd = listConst.length - 1;
    uint16_t valueIndex = listConst.constants[listConstEnd - i];

    seq = _allocateCons(vm, alreadyHydratedConstants[valueIndex], seq, meta);
  }

  return seq;
}

// TODO: I had a thought, can we get rid of CodeUnit entirely and just replace it with FnConstant?
// TODO: I had another thought, can we get rid of the nested graph of constants and flatten it entirely?

Value hydrateConstant(VM *vm, Value *alreadyHydratedConstants, Constant c) {

  Value v;

  switch (c.type) {
    case CT_BOOL:
      v = wrapBool(c.boolean);
      break;
    case CT_INT:
      v = wrapUint(c.integer);
      break;
    case CT_NIL:
      v = nil();
      break;
    case CT_FN:
      v = fnHydrate(vm, &c.function);
      break;
    case CT_STR:
      v = stringHydrate(vm, c.string.value, c.string.length);
      break;
    case CT_SYMBOL:
      v = symbolHydrate(vm, c.symbol);
      break;
    case CT_KEYWORD:
      v = keywordHydrate(vm, c.keyword);
      break;
    case CT_LIST:
      v = listHydrate(vm, alreadyHydratedConstants, c.list);
      break;
    case CT_NONE:
    default:
      explode("invalid constant: %u", c.type);
  }

  return v;
}

void hydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value *values) {
  for (uint16_t i=0; i<numConstants; i++) {
    Constant c = constants[i];
    values[i] = hydrateConstant(vm, values, c);
  }
}

/*
 * Create a reader representation of a Value (an Expr).
 *
 * Some representations are approximate and cannot be round-tripped through eval, such as functions and closures.
 */
RetVal tryVMPrn(VM *vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  RetVal ret;

  exprInitContents(expr);

  ValueType resultType = valueType(result);

  TryPrn prn = vm->valueTypeTable.valueTypes[resultType].tryPrn;
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
  var->value = nil();
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

  ValueType nextType = valueType(next);
  if (nextType != VT_NIL && nextType != VT_LIST) {
    explode("a Cons next must be nil or a list: %s", getValueTypeName(vm, nextType));
  }

  Cons *cons = NULL;

  uint64_t size = padAllocSize(sizeof(Cons));

  cons = alloc(vm, size);

  consInitContents(cons);
  cons->header = makeObjectHeader(W_LIST_TYPE, size);
  cons->value = value;
  cons->next = next;

  return (Value)cons;
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

  ValueType fnRefType = valueType(invocable->fnRef);
  switch (fnRefType) {
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
          getValueTypeName(vm, fnRefType));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryPreprocessArguments(VM *vm, Frame_t parent, uint16_t numArgs, bool usesVarArgs, Error *error) {

  RetVal ret;

  Value numArgsSuppliedValue = popOperand(parent);
  if (valueType(numArgsSuppliedValue) != VT_UINT) {
    explode("first op stack value must be number of arguments supplied: %s",
        getValueTypeName(vm, valueType(numArgsSuppliedValue)));
  }

  uint64_t numArgsSupplied = unwrapUint(numArgsSuppliedValue);

  if (!usesVarArgs) {
    if (numArgsSupplied != numArgs) {
      throwRuntimeError(error, "required arguments not supplied, expected %u but got %" PRIu64, numArgs,
          numArgsSupplied);
    }
  }
  else {

    uint16_t numVarArgs;
    if (numArgsSupplied > numArgs) {
      numVarArgs = (numArgsSupplied - numArgs) + 1;
    }
    else if (numArgsSupplied == numArgs) {
      numVarArgs = 1;
    }
    else if (numArgsSupplied == numArgs - 1) {
      numVarArgs = 0;
    }
    else {
      throwRuntimeError(error, "required arguments not supplied, expected %u or more arguments but got %" PRIu64,
                        numArgs - 1, numArgsSupplied);
    }

    Value seq = nil();
    pushFrameRoot(vm, &seq);

    // read the extra args into that sequence, push it back on the stack
    for (uint16_t i = 0; i < numVarArgs; i++) {
      Cons *cons = deref(&vm->gc, allocateCons(vm, nil(), nil()));
      cons->value = popOperand(parent);
      cons->next = seq;
      seq = (Value)cons;
    }

    popFrameRoot(vm);
    pushOperand(parent, seq);
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

  if (valueType(pop) == VT_CFN) {
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

  if (valueType(pop) == VT_CFN) {
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

  Value a = popOperand(frame);
  Value b = popOperand(frame);

  Value c = wrapBool(a == b);
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

  if (valueType(a) != VT_UINT) {
    throwRuntimeError(error, "can only add integers: %s", getValueTypeName(vm, valueType(a)));
  }
  if (valueType(b) != VT_UINT) {
    throwRuntimeError(error, "can only add integers: %s", getValueTypeName(vm, valueType(b)));
  }

  Value c = wrapUint(unwrapUint(a) + unwrapUint(b));
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

  if (valueType(a) != VT_UINT) {
    throwRuntimeError(error, "can only subtract integers: %s", getValueTypeName(vm, valueType(a)));
  }
  if (valueType(b) != VT_UINT) {
    throwRuntimeError(error, "can only subtract integers: %s", getValueTypeName(vm, valueType(b)));
  }

  Value c = wrapUint(unwrapUint(a) - unwrapUint(b));
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
  ValueType varNameType = valueType(varName);

  if (varNameType != VT_STR) {
    throwRuntimeError(error, "expected a string: %s", getValueTypeName(vm, varNameType));
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
  cl->header = 0;
  cl->fn = nil();
  cl->numCaptures = 0;
  cl->capturesOffset = 0;
}

// (8), offset (16) | (captures... -> value)
RetVal tryLoadClosureEval(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex = readIndex(frame);
  Value fnValue = getConst(frame, constantIndex);

  ValueType fnValueType = valueType(fnValue);
  if (fnValueType != VT_FN) {
    throwRuntimeError(error, "cannot create a closure from this value type: %s",
        getValueTypeName(vm, fnValueType));
  }

  Fn *fn = deref(&vm->gc, fnValue);

  Closure *closure = NULL;

  uint64_t capturesSize = fn->numCaptures * sizeof(Value);
  uint64_t clSize = padAllocSize(sizeof(Closure) + capturesSize);

  closure = alloc(vm, clSize);
  Value closureValue = (Value)closure;

  closureInitContents(closure);
  closure->header = makeObjectHeader(W_CLOSURE_TYPE, clSize);
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

  ValueType seqType = valueType(seq);
  if (seqType != VT_NIL && seqType != VT_LIST) {
    throwRuntimeError(error, "cannot cons onto a value of type %s", getValueTypeName(vm, seqType));
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
  ValueType seqType = valueType(seq);

  Value result;

  if (seqType == VT_NIL) {
    result = nil();
  }
  else if (seqType == VT_LIST) {
    Cons *cons = deref(&vm->gc, seq);
    result = cons->value;
  }
  else {
    throwRuntimeError(error, "cannot get first from a value of type %s", getValueTypeName(vm, seqType));
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
  ValueType seqType = valueType(seq);

  Value result;

  if (seqType == VT_NIL) {
    result = nil();
  }
  else if (seqType == VT_LIST) {
    Cons *cons = deref(&vm->gc, seq);
    result = cons->next;
  }
  else {
    throwRuntimeError(error, "cannot get rest from a value of type %s", getValueTypeName(vm, seqType));
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
  ValueType strValueType = valueType(strValue);

  wchar_t *sym = NULL;
  uint64_t symLength = 0;

  if (strValueType == VT_STR) {
    String *str = deref(&vm->gc, strValue);
    sym = stringValue(str);
    symLength = str->length;
  }
  else if (strValueType == VT_SYMBOL) {
    Symbol *s = deref(&vm->gc, strValue);
    String *str = deref(&vm->gc, s->name);
    sym = stringValue(str);
    symLength = str->length;
  }
  else {
    throwRuntimeError(error, "only strings or symbols can identify vars: %s", getValueTypeName(vm, strValueType));
  }

  Var *var;
  if (!resolveVar(&vm->namespaces, sym, symLength, &var)) {
    throwRuntimeError(error, "no such var exists: %ls", sym);
  }

  if (!var->isMacro) {
    if (valueType(var->value) != VT_FN) {
      throwRuntimeError(error, "only vars referring to functions can be macros: %ls -> %s",
          sym, getValueTypeName(vm, valueType(var->value)));
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
  ValueType type = valueType(strValue);

  wchar_t *sym = NULL;
  uint64_t symLength = 0;

  if (type == VT_STR) {
    String *str = deref(&vm->gc, strValue);
    sym = stringValue(str);
    symLength = str->length;
  }
  else if (type == VT_SYMBOL) {
    Symbol *s = deref(&vm->gc, strValue);
    String *str = deref(&vm->gc, s->name);
    sym = stringValue(str);
    symLength = str->length;
  }
  else {
    throwRuntimeError(error, "only strings or symbols can identify vars: %s", getValueTypeName(vm, type));
  }

  Value result = wrapBool(false);

  Var *var;
  if (resolveVar(&vm->namespaces, sym, symLength, &var)) {
    result = wrapBool(var->isMacro);
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
  Value typeId = wrapUint(valueType(value));
  pushOperand(frame, typeId);

  return R_SUCCESS;
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
bool isTruthyBool(Value v) { return unwrapBool(v); }

void relocateChildrenFn(VM_t vm, void *obj) {
  Fn *fn = obj;
  for (uint16_t i=0; i<fn->numConstants; i++) {
    relocate(vm, &fnConstants(fn)[i]);
  }
}

void relocateChildrenList(VM_t vm, void *obj) {
  Cons *cons = obj;
  relocate(vm, &cons->value);
  relocate(vm, &cons->next);
}

void relocateChildrenClosure(VM_t vm, void *obj) {
  Closure *closure = obj;
  relocate(vm, &closure->fn);
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    relocate(vm, &closureCaptures(closure)[i]);
  }
}

RetVal tryPrnNil(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_NIL;
  return R_SUCCESS;
}

RetVal tryPrnInt(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_NUMBER;
  expr->number.value = unwrapUint(result);
  return R_SUCCESS;
}

RetVal tryPrnBool(VM_t vm, Value result, Pool_t pool, Expr *expr, Error *error) {
  expr->type = N_BOOLEAN;
  expr->boolean.value = unwrapBool(result);
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
  String *str = deref(&vm->gc, sym->name);

  expr->type = N_SYMBOL;
  expr->symbol.length = str->length;
  throws(tryCopyText(pool, stringValue(str), &expr->symbol.value, expr->string.length, error));

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
  return valueType(value) == VT_NIL;
}

typedef struct Property {
  Keyword *key;
  Value value;
} Property;

RetVal tryReadProperty(VM *vm, Value *ptr, Property *p, Error *error) {
  RetVal ret;

  if (valueType(*ptr) != VT_LIST) {
    throwRuntimeError(error, "expected property list: %s",
                      getValueTypeName(vm, valueType(*ptr)));
  }

  Cons *properties = deref(&vm->gc, *ptr);

  if (valueType(properties->value) != VT_KEYWORD) {
    throwRuntimeError(error, "expected keyword for property key: %s",
                      getValueTypeName(vm, valueType(properties->value)));
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

      if (valueType(p.value) != VT_UINT) {
        throwRuntimeError(error, "expected line-number property value to be an int: %s",
                          getValueTypeName(vm, valueType(p.value)));
      }

      expr->source.isSet = true;
      expr->source.lineNumber = unwrapUint(p.value);
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

  throws(tryPrnMetadata(vm, cons->metadata, expr, error));

  listInitContents(&expr->list);
  Expr *elem;

  tryPalloc(pool, elem, sizeof(Expr), "Expr");
  exprInitContents(elem);

  throws(tryVMPrn(vm, cons->value, pool, elem, error));
  throws(tryListAppend(pool, &expr->list, elem, error));

  while (valueType(cons->next) != VT_NIL) {

    if (valueType(cons->next) != VT_LIST) {
      throwRuntimeError(error, "this should always be a type of VT_LIST: %s",
                        getValueTypeName(vm, valueType(cons->next)));
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

bool equals(VM_t vm, Value this, Value that);

bool equalsStr(VM_t vm, Value this, Value that) {

  if (valueType(this) == valueType(that)) {
    return false;
  }
  else {

    String *a = deref(&vm->gc, this);
    String *b = deref(&vm->gc, that);

    if (a == b) {
      return true;
    }

    if (a->length != b->length) {
      return false;
    }
    else {
      return wcscmp(stringValue(a), stringValue(b)) == 0;
    }
  }
}

bool equalsSymbol(VM_t vm, Value this, Value that) {

  if (valueType(this) == valueType(that)) {
    return false;
  }
  else {
    return this == that;
  }
}

bool equalsKeyword(VM_t vm, Value this, Value that) {

  if (valueType(this) == valueType(that)) {
    return false;
  }
  else {

    Keyword *a = deref(&vm->gc, this);
    Keyword *b = deref(&vm->gc, that);

    if (a == b) {
      return true;
    }

    if (a->length != b->length) {
      return false;
    }
    else {
      return wcscmp(keywordValue(a), keywordValue(b)) == 0;
    }
  }
}

bool equalsList(VM_t vm, Value this, Value that) {

  if (valueType(this) == valueType(that)) {
    return false;
  }
  else {
    bool e = true;
    while (valueType(this) != T_NIL) {

      Cons *a = deref(&vm->gc, this);
      Cons *b = deref(&vm->gc, that);

      if (a == b) {
        return true;
      }

      if (!equals(vm, a->value, b->value)) {
        e = false;
        break;
      }

      bool nextTypesMismatched = valueType(a->next) != valueType(b->next);
      if (nextTypesMismatched) {
        e = false;
        break;
      }

      this = a->next;
      that = b->next;
    }

    return e;
  }
}

bool equals(VM_t vm, Value this, Value that) {
  ValueType thisType = valueType(this);
  Equals equals = vm->valueTypeTable.valueTypes[thisType].equals;
  if (equals == NULL) {
    // assume immediate value
    return this == that;
  }
  else {
    return equals(vm, this, that);
  }
}

ValueTypeTable valueTypeTableCreate() {
  ValueTypeTable table;

  // init table with blanks
  uint16_t valueTypesAllocated = sizeof(table.valueTypes) / sizeof(table.valueTypes[0]);
  for (int i=0; i<valueTypesAllocated; i++) {
    table.valueTypes[i].name = NULL;
    table.valueTypes[i].isTruthy = NULL;
    table.valueTypes[i].relocateChildren = NULL;
    table.valueTypes[i].tryPrn = NULL;
  }

  // init with known value types
  ValueTypeInfo valueTypes [] = {
      [VT_NIL]       = {.name = "nil",
                        .isTruthy = &isTruthyNo,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnNil,
                        .equals = NULL},
      [VT_UINT]      = {.name = "uint",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnInt,
                        .equals = NULL},
      [VT_BOOL]      = {.name = "bool",
                        .isTruthy = &isTruthyBool,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnBool,
                        .equals = NULL},
      [VT_FN]        = {.name = "fn",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenFn,
                        .tryPrn = &tryPrnFn,
                        .equals = NULL},
      [VT_STR]       = {.name = "str",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnStr,
                        .equals = &equalsStr},
      [VT_SYMBOL]    = {.name = "symbol",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnSymbol,
                        .equals = &equalsSymbol},
      [VT_KEYWORD]   = {.name = "keyword",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnKeyword,
                        .equals = &equalsKeyword},
      [VT_LIST]      = {.name = "list",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenList,
                        .tryPrn = &tryPrnList,
                        .equals = &equalsList},
      [VT_CLOSURE]   = {.name = "closure",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenClosure,
                        .tryPrn = &tryPrnClosure,
                        .equals = NULL},
      [VT_CFN]       = {.name = "cfn",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .tryPrn = &tryPrnCFn,
                        .equals = NULL}
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

typedef struct FrameRoot FrameRoot;

typedef struct FrameRoot {
  Value* valuePtr;
  FrameRoot *next;
} FrameRoot;

void frameRootInitContents(FrameRoot *root) {
  root->valuePtr = NULL;
  root->next = NULL;
}

typedef struct Frame {
  Frame *parent;

  Value fnRef;
  Fn *fn;

  Value *locals;

  uint64_t opStackMaxDepth;
  uint64_t opStackUsedDepth;
  Value *opStack;

  FrameRoot *roots;

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
  frame->result = nil();
  frame->pc = 0;
  handlerInitContents(&frame->handler);
  frame->handlerSet = false;
  exceptionInitContents(&frame->exception);
  frame->exceptionSet = false;

  frame->opStackUsedDepth = 0;
  frame->opStackMaxDepth = 0;
  frame->opStack = NULL;

  frame->roots = NULL;
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

FrameRoot_t frameRoots(VM *vm) {
  return vm->current->roots;
}

Value* frameRootValue(FrameRoot_t root) {
  return root->valuePtr;
}

FrameRoot_t frameRootNext(FrameRoot_t root) {
  return root->next;
}

void pushFrameRoot(VM *vm, Value *rootPtr) {
  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  FrameRoot *root = stackAllocate(stack, sizeof(FrameRoot), "FrameRoot");
  root->valuePtr = rootPtr;
  root->next = frame->roots;
  frame->roots = root;
}

void popFrameRoot(VM *vm) {
  Frame *frame = vm->current;
  frame->roots = frame->roots->next;
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

  Value fnRef = fnHydrate(vm, &c);

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
  if (valueType(value) != VT_LIST && valueType(value) != VT_NIL) { \
    throwRuntimeError(error, "expected a list type: %s", getValueTypeName(vm, valueType(value))); \
  } \
}

#define ASSERT_STR(value, ...) {\
  if (valueType(value) != VT_STR) { \
    throwRuntimeError(error, "expected a string type: %s", getValueTypeName(vm, valueType(value))); \
  } \
}

Value stringMakeBlank(VM *vm, uint64_t length) {

  String *str = NULL;

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t strSize = padAllocSize(sizeof(String) + textSize);

  str = alloc(vm, strSize);

  stringInitContents(str);
  str->header = makeObjectHeader(W_STR_TYPE, strSize);
  str->length = length;

  str->valueOffset = sizeof(String);
  stringValue(str)[length] = L'\0';

  return (Value)str;
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
  pushFrameRoot(vm, &strings);

  uint64_t totalLength = 0;

  Value cursor = strings;
  while (valueType(cursor) != VT_NIL) {

    Cons *seq = deref(&vm->gc, cursor);

    ASSERT_STR(seq->value);

    String *string = deref(&vm->gc, seq->value);
    totalLength += string->length;

    cursor = seq->next;
  }

  Value resultRef = stringMakeBlank(vm, totalLength);
  String *result = deref(&vm->gc, resultRef);

  uint64_t totalSizeWritten = 0;

  cursor = strings;
  while (valueType(cursor) != VT_NIL) {
    Cons *seq = deref(&vm->gc, cursor);

    String *string = deref(&vm->gc, seq->value);

    wchar_t *writePtr = (void*)result + result->valueOffset + totalSizeWritten;
    size_t textSize = string->length * sizeof(wchar_t);
    memcpy(writePtr, stringValue(string), textSize);
    totalSizeWritten += textSize;

    cursor = seq->next;
  }

  popFrameRoot(vm);
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

  Value resultRef = stringMakeBlank(vm, stringBufferLength(b));
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

Value symbolMakeBlank(VM *vm) {
  Symbol *sym = NULL;

  uint64_t size = padAllocSize(sizeof(Symbol));
  sym = alloc(vm, size);

  symbolInitContents(sym);
  sym->header = makeObjectHeader(W_SYMBOL_TYPE, size);

  return (Value)sym;
}

RetVal trySymbolBuiltin(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);
  pushFrameRoot(vm, &value);

  ASSERT_STR(value);

  Value result = symbolMakeBlank(vm);

  Symbol *sym = deref(&vm->gc, result);
  sym->name = value;

  popFrameRoot(vm);
  pushOperand(frame, result);

  return R_SUCCESS;
  failure:
  return ret;
}

Value keywordMakeBlank(VM *vm, uint64_t length) {
  Keyword *kw = NULL;

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t size = padAllocSize(sizeof(Keyword) + textSize);

  kw = alloc(vm, size);

  keywordInitContents(kw);
  kw->header = makeObjectHeader(W_KEYWORD_TYPE, size);
  kw->length = length;

  kw->valueOffset = sizeof(Symbol);
  keywordValue(kw)[kw->length] = L'\0';

  return (Value)kw;
}

RetVal tryKeywordBuiltin(VM *vm, Frame_t frame, Error *error) {
  RetVal ret;

  Value value = popOperand(frame);
  ASSERT_STR(value);
  pushFrameRoot(vm, &value);

  uint64_t length = 0;
  {
    String *string = deref(&vm->gc, value);
    length = string->length;
  }

  Value result = keywordMakeBlank(vm, length);

  // actually copy string into symbol
  String *string = deref(&vm->gc, value);
  Keyword *kw = deref(&vm->gc, result);
  memcpy(keywordValue(kw), stringValue(string), kw->length * sizeof(wchar_t));

  pushOperand(frame, result);
  popFrameRoot(vm);

  return R_SUCCESS;
  failure:
  return ret;
}

void cFnInitContents(CFn *fn) {
  fn->header = 0;
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

  fn = alloc(vm, fnSize);
  Value value = (Value)fn;

  cFnInitContents(fn);
  fn->header = makeObjectHeader(W_CFN_TYPE, fnSize);
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
