#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>
#include <setjmp.h>
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
  VT_ARRAY,
  VT_MAP,
  VT_MAP_ENTRY
} ValueType;

#define W_GC_FORWARDING_BIT      0x8000000000000000L   /* header contains forwarding pointer */
#define W_BYTEBLOCK_BIT          0x4000000000000000L   /* block contains bytes instead of slots */
#define W_SPECIALBLOCK_BIT       0x2000000000000000L   /* 1st item is a non-value */
#define W_8ALIGN_BIT             0x1000000000000000L   /* data is aligned to 8-byte boundary */
#define W_HEADER_TYPE_BITS       0x0f00000000000000L
#define W_HEADER_SIZE_MASK       0x00ffffffffffffffL

#define W_FN_TYPE        0x0u
#define W_STR_TYPE       0x1u
#define W_SYMBOL_TYPE    0x2u
#define W_KEYWORD_TYPE   0x3u
#define W_LIST_TYPE      0x4u
#define W_CLOSURE_TYPE   0x5u
#define W_CFN_TYPE       0x6u
#define W_ARRAY_TYPE     0x7u
#define W_MAP_TYPE       0x8u
#define W_MAP_ENTRY_TYPE 0x9u

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

uint64_t objectHeaderSizeBytes(ObjectHeader h) {
  uint8_t type = objectHeaderType(h);
  switch (type) {
    case W_ARRAY_TYPE: {
      uint64_t size = h & W_HEADER_SIZE_MASK;
      return sizeof(ObjectHeader) + (size * sizeof(Value));
    }
    default:
      return h & W_HEADER_SIZE_MASK;
  }
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
    case W_ARRAY_TYPE: return VT_ARRAY;
    case W_MAP_TYPE: return VT_MAP;
    case W_MAP_ENTRY_TYPE: return VT_MAP_ENTRY;
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
typedef void (*CFnInvoke) (VM_t vm, Frame_t frame);

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
  bool valueDefined;
  Value name;
  bool isMacro;
} Symbol;

typedef struct Keyword {
  ObjectHeader header;

  Value name;
} Keyword;

typedef struct Cons {
  ObjectHeader header;

  Value metadata;
  Value value;
  Value next; // this must be a Cons, or Nil
} Cons;

typedef struct Array {
  ObjectHeader header;
} Array;

typedef struct MapEntry {
  ObjectHeader header;
  bool used;
  Value key;
  uint32_t keyHash;
  Value value;
} MapEntry;

typedef struct Map {
  ObjectHeader header;
  uint64_t size;
  Value entries;
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

// instruction definitions

typedef void (*Eval) (struct VM *vm, Frame_t frame);

typedef struct Inst {
  const char *name;
  void (*print)(int *i, const char* name, uint8_t *code);
  Eval eval;
} Inst;

typedef struct InstTable {
  uint8_t numInstructions;
  Inst instructions[256];
} InstTable;

// value type definitions

typedef struct Invocable {
  Value ref;
  Fn *fn;
  Closure *closure;
} Invocable;

typedef void (*RelocateChildren)(VM_t vm, void *obj);
typedef void (*Prn)(VM_t vm, Value result, Expr *expr);

typedef struct ValueTypeInfo {
  const char *name;
  bool (*isTruthy)(Value value);
  RelocateChildren relocateChildren;
  Prn prn;
} ValueTypeInfo;

typedef struct ValueTypeTable {
  uint8_t numValueTypes;
  ValueTypeInfo valueTypes[256];
} ValueTypeTable;

// vm state

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

typedef struct TableEntry {
  bool used;
  Value name;
  uint32_t nameHash;
  Value value;
} TableEntry;

typedef struct Table {
  uint64_t size;
  uint64_t numAllocatedEntries;
  TableEntry *entries;
  // load
} Table;

typedef struct VM {
  VMConfig config;
  GC gc;
  InstTable instTable;
  ValueTypeTable valueTypeTable;
  Stack stack;
  FrameRoot_t noFrameRoots;
  Frame_t current;
  Table symbolTable;
  Table keywordTable;
  Pool_t outputPool; // this is mutable, it changes on every eval request
  jmp_buf jumpBuf;
  VMException *exception;
} VM;

/*
 * SymbolTable
 */

#define SYMBOL_TABLE_MIN_ENTRIES 16
#define SYMBOL_TABLE_MIN_LOAD .40
#define SYMBOL_TABLE_MAX_LOAD .70

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

void tableEntryInitContents(TableEntry *e) {
  e->used = false;
  e->name = nil();
  e->nameHash = 0;
  e->value = nil();
}

void tableInitContents(Table *t) {
  t->numAllocatedEntries = 0;
  t->entries = NULL;
  t->size = 0;
}

void tableFreeContents(Table *t) {
  if (t != NULL) {
    free(t->entries);
    t->numAllocatedEntries = 0;
    t->entries = NULL;
    t->size = 0;
  }
}

void tableInit(Table *table) {

  tableInitContents(table);

  table->numAllocatedEntries = SYMBOL_TABLE_MIN_ENTRIES;
  table->entries = malloc(sizeof(TableEntry) * table->numAllocatedEntries);
  if (table->entries == NULL) {
    explode("failed to allocate TableEntries array");
  }
  for (uint64_t i=0; i<table->numAllocatedEntries; i++) {
    tableEntryInitContents(&table->entries[i]);
  }
}

void* deref(GC *gc, Value value);

TableEntry* findEntry(VM *vm, Table *table, String* name, uint32_t hash) {

  uint64_t index = hash % table->numAllocatedEntries;

  for (uint64_t i = index; i < table->numAllocatedEntries; i++) {
    TableEntry *entry = &table->entries[i];

    if (!entry->used) {
      return entry;
    } else {
      String *thisName = deref(&vm->gc, entry->name);
      if (wcscmp(stringValue(name), stringValue(thisName)) == 0) {
        return entry;
      }
    }
  }

  for (uint64_t i = 0; i < index; i++) {
    TableEntry *entry = &table->entries[i];

    if (!entry->used) {
      return entry;
    } else {
      String *thisName = deref(&vm->gc, entry->name);
      if (wcscmp(stringValue(name), stringValue(thisName)) == 0) {
        return entry;
      }
    }
  }

  explode("could not find an available TableEntry");
}

Value tableLookup(VM *vm, Table *table, Value name) {

  if (valueType(name) != VT_STR) {
    explode("names must be strings");
  }

  String *s = deref(&vm->gc, name);
  uint32_t hash = stringHash(s);

  TableEntry *found = findEntry(vm, table, s, hash);

  if (found->used) {
    return found->value;
  }
  else {
    return nil();
  }
}

void _putEntryWithHash(VM *vm, Table *table, Value insertMe, Value name, uint32_t hash) {
  TableEntry *found = findEntry(vm, table, deref(&vm->gc, name), hash);
  if (!found ->used) {
    table->size++;
    found->used = true;
  }
  found->name = name;
  found->nameHash = hash;
  found->value = insertMe;
}

void _putEntry(VM *vm, Table *table, Value name, Value insertMe) {
  String *s = deref(&vm->gc, name);
  uint32_t hash = stringHash(s);
  _putEntryWithHash(vm, table, insertMe, name, hash);
}

void putEntry(VM *vm, Table *table, Value name, Value insertMe) {

  _putEntry(vm, table, name, insertMe);

  float load = (float)table->size / (float)table->numAllocatedEntries;

  // resize
  if (load > SYMBOL_TABLE_MAX_LOAD || (load > SYMBOL_TABLE_MIN_ENTRIES && load < SYMBOL_TABLE_MIN_LOAD)) {

    uint64_t newAllocatedEntries;
    if (load > SYMBOL_TABLE_MAX_LOAD) {
      newAllocatedEntries = table->numAllocatedEntries * 2;
    }
    else {
      newAllocatedEntries = table->numAllocatedEntries / 2;
      if (newAllocatedEntries < SYMBOL_TABLE_MIN_ENTRIES) {
        newAllocatedEntries = SYMBOL_TABLE_MIN_ENTRIES;
      }
    }

    uint64_t numOldEntries = table->numAllocatedEntries;
    TableEntry *oldEntries = table->entries;

    table->size = 0;
    table->numAllocatedEntries = newAllocatedEntries;
    table->entries = malloc(sizeof(TableEntry) * newAllocatedEntries);
    if (table->entries == NULL) {
      explode("failed to allocate TableEntries array");
    }
    for (uint64_t i=0; i<table->numAllocatedEntries; i++) {
      tableEntryInitContents(&table->entries[i]);
    }

    for (uint64_t i=0; i<numOldEntries; i++) {
      TableEntry *oldEntry = &oldEntries[i];
      if (oldEntry->used) {
        _putEntryWithHash(vm, table, oldEntry->value, oldEntry->name, oldEntry->nameHash);
      }
    }

    free(oldEntries);
  }
}

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

bool hasException(VM *vm);
void setException(VM *vm, VMException *e);
VMException* getException(VM *vm);

Frame_t pushFrame(VM *vm, Value newFn);
Frame_t replaceFrame(VM *vm, Value newFn);
Frame_t popFrame(VM *vm);

void pushFrameRoot(VM *vm, Value *rootPtr);
void popFrameRoot(VM *vm);
FrameRoot_t frameRoots(Frame_t frame);
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
 * alloc/gc impl
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

  if (length < sizeof(ObjectHeader)) {
    explode("alloc length is too small, %" PRIu64, length);
  }

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

  if (!vm->config.gcOnAlloc) {
    int success = _alloc(&vm->gc, length, &ptr);
    if (success == R_OOM) {
      collect(vm);
      success = _alloc(&vm->gc, length, &ptr);
      if (success == R_OOM) {
        explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
      }
    }
  }
  else {
    collect(vm);
    int success = _alloc(&vm->gc, length, &ptr);
    if (success == R_OOM) {
      explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

  return ptr;
}

uint64_t padAllocSize(uint64_t length) {
  uint16_t rem = length % 4;
  if (rem != 0) {
    return length + (4 - rem);
  }
  else {
    return length;
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
    uint64_t size = objectHeaderSizeBytes(*header);

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

void relocateTable(VM *vm, Table *table) {
  for (uint64_t i=0; i<table->numAllocatedEntries; i++) {
    TableEntry *entry = &table->entries[i];
    if (entry->used) {
      relocate(vm, &entry->name);
      relocate(vm, &entry->value);
    }
  }
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

  // relocate tables
  relocateTable(vm, &vm->symbolTable);
  relocateTable(vm, &vm->keywordTable);

  // relocate noFrameRoots
  FrameRoot_t noFrameRoot = vm->noFrameRoots;
  while (noFrameRoot != NULL) {
    Value *valuePtr = frameRootValue(noFrameRoot);
    relocate(vm, valuePtr);
    noFrameRoot = frameRootNext(noFrameRoot);
  }

  // relocate call stack roots
  Frame_t current = vm->current;
  if (current != NULL) {
    while (true) {

      // relocate fnRef
      {
        Value oldFnRef = getFnRef(current);
        Value newFnRef = oldFnRef;
        relocate(vm, &newFnRef);
        setFnRef(vm, current, newFnRef);
      }

      uint16_t locals = numLocals(current);
      for (uint16_t i = 0; i < locals; i++) {
        Value *val = getLocalRef(current, i);
        relocate(vm, val);
      }

      uint64_t operands = numOperands(current);
      for (uint64_t i = 0; i < operands; i++) {
        Value *val = getOperandRef(current, i);
        relocate(vm, val);
      }

      FrameRoot_t root = frameRoots(current);
      while (root != NULL) {
        Value *valuePtr = frameRootValue(root);
        relocate(vm, valuePtr);
        root = frameRootNext(root);
      }

      if (!hasParent(current)) {
        break;
      } else {
        current = getParent(current);
      }
    }
  }

  void *scanptr = vm->gc.currentHeap;

  // relocate all the objects this object references
  while (scanptr < vm->gc.allocPtr) {
    ObjectHeader *header = scanptr;

    uint8_t type = objectHeaderValueType(*header);
    uint64_t sizeBytes = objectHeaderSizeBytes(*header);

    relocateChildren(vm, type, scanptr);
    scanptr += sizeBytes;
  }

  uint64_t newHeapUsed = vm->gc.allocPtr - vm->gc.currentHeap;
  uint64_t sizeRecovered = oldHeapUsed - newHeapUsed;
  uint64_t end = now();
  uint64_t duration = end - start;

  printf("gc: completed, %" PRIu64 " bytes recovered, %" PRIu64 " bytes used, took %" PRIu64 "ms\n", sizeRecovered, newHeapUsed, duration);
}

/*
 * Loading Constants as Values
 */

Value hydrateConstant(VM *vm, Fn **protectedFn, Constant c);

void fnInitContents(Fn *fn) {

  fn->header = 0;

  fn->hasName = false;
  fn->nameLength = 0;
  fn->nameOffset = 0;

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

  Fn *fn = NULL;
  {
    uint64_t nameSize = (fnConst->name.length + 1) * sizeof(wchar_t);
    uint64_t constantsSize = fnConst->numConstants * sizeof(Value);
    uint64_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
    uint64_t sourceFileNameSize = (fnConst->code.sourceTable.fileName.length + 1) * sizeof(wchar_t);
    uint64_t lineNumbersSize = fnConst->code.sourceTable.numLineNumbers * sizeof(LineNumber);

    uint64_t fnSize = padAllocSize(
        sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize + lineNumbersSize);

    fn = alloc(vm, fnSize);

    fnInitContents(fn);

    fn->header = makeObjectHeader(W_FN_TYPE, fnSize);

    fn->nameOffset = sizeof(Fn);
    fn->constantsOffset = sizeof(Fn) + nameSize;
    fn->codeOffset = sizeof(Fn) + nameSize + constantsSize;
    fn->sourceFileNameOffset = sizeof(Fn) + nameSize + constantsSize + codeSize;
    fn->lineNumbersOffset = sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize;

    fn->hasName = fnConst->hasName;
    if (fn->hasName) {
      fn->nameLength = fnConst->name.length;
      size_t copySize = fnConst->name.length * sizeof(wchar_t);

      memcpy(fnName(fn), fnConst->name.value, copySize);
      fnName(fn)[fn->nameLength] = L'\0';
    }

    fn->numArgs = fnConst->numArgs;
    fn->usesVarArgs = fnConst->usesVarArgs;

    fn->numConstants = fnConst->numConstants;

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
  }

  for (uint16_t i=0; i<fn->numConstants; i++) {
    fnConstants(fn)[i] = nil();
  }

  pushFrameRoot(vm, (Value*)&fn);
  for (uint16_t i=0; i<fn->numConstants; i++) {
    Value hydrated = hydrateConstant(vm, &fn, fnConst->constants[i]);
    fnConstants(fn)[i] = hydrated;
  }
  popFrameRoot(vm);

  return (Value)fn;
}

void stringInitContents(String *s) {
  s->header = 0;
  s->length = 0;
  s->valueOffset = 0;
  s->hash = 0;
}

Value stringHydrate(VM *vm, wchar_t *text, uint64_t length) {

  uint64_t textSize = (length + 1) * sizeof(wchar_t);
  uint64_t strSize = padAllocSize(sizeof(String) + textSize);

  String *str = alloc(vm, strSize);
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
  s->valueDefined = false;
  s->name = nil();
  s->isMacro = false;
}

Value symbolIntern(VM *vm, Value *protectedName) {

  Table *table = &vm->symbolTable;
  Value result = tableLookup(vm, table, *protectedName);
  if (valueType(result) == VT_NIL) {

    uint64_t size = padAllocSize(sizeof(Symbol));
    Symbol *symbol = alloc(vm, size);

    symbolInitContents(symbol);
    symbol->header = makeObjectHeader(W_SYMBOL_TYPE, size);
    symbol->name = *protectedName;

    putEntry(vm, table, *protectedName, (Value) symbol);
    result = (Value)symbol;
  }

  return result;
}

Value symbolHydrate(VM *vm, SymbolConstant symConst) {

  Value protectedName = stringHydrate(vm, symConst.value, symConst.length);
  pushFrameRoot(vm, &protectedName);

  Value value = symbolIntern(vm, &protectedName);

  popFrameRoot(vm);
  return value;
}

void keywordInitContents(Keyword *k) {
  k->header = 0;
  k->name = nil();
}

Value keywordIntern(VM *vm, Value *protectedName) {

  Table *table = &vm->keywordTable;
  Value result = tableLookup(vm, table, *protectedName);
  if (valueType(result) == VT_NIL) {

    uint64_t size = padAllocSize(sizeof(Keyword));
    Keyword *kw = alloc(vm, size);

    keywordInitContents(kw);
    kw->header = makeObjectHeader(W_KEYWORD_TYPE, size);
    kw->name = *protectedName;

    putEntry(vm, table, *protectedName, (Value)kw);
    result = (Value)kw;
  }

  return result;
}

Value keywordHydrate(VM *vm, KeywordConstant kwConst) {
  Value protectedName = stringHydrate(vm, kwConst.value, kwConst.length);
  pushFrameRoot(vm, &protectedName);

  Value value = keywordIntern(vm, &protectedName);

  popFrameRoot(vm);
  return value;
}

void consInitContents(Cons *c) {
  c->header = 0;
  c->metadata = nil();
  c->value = nil();
  c->next = nil();
}

Cons* makeCons(VM *vm) {

  Cons *cons = NULL;
  uint64_t size = padAllocSize(sizeof(Cons));
  cons = alloc(vm, size);

  consInitContents(cons);
  cons->header = makeObjectHeader(W_LIST_TYPE, size);

  return cons;
}

/*
 * `protectedFn` is a alloc-safe pointer to the Fn for which a constant is being hydrated.
 * This is included so we so that references to already-hydrated values can be resolved by
 * constant index.
 */
Value listHydrate(VM *vm, Fn **protectedFn, ListConstant listConst) {

  // build up meta property list with conses
  Value protectedMeta = nil();
  pushFrameRoot(vm, &protectedMeta);

  for (uint64_t i=0; i<listConst.meta.numProperties; i++) {
    ConstantMetaProperty *p = &listConst.meta.properties[i];

    Cons *propValue = makeCons(vm);
    propValue->value = fnConstants(*protectedFn)[p->valueIndex];
    propValue->next = protectedMeta;
    protectedMeta = (Value)propValue;

    Cons *propKey = makeCons(vm);
    propKey->value = fnConstants(*protectedFn)[p->keyIndex];
    propKey->next = protectedMeta;
    protectedMeta = (Value)propKey;
  }

  // build up list with conses, each cons gets the same meta
  Value protectedSeq = nil();
  pushFrameRoot(vm, &protectedSeq);

  for (uint16_t i = 0; i < listConst.length; i++) {

    uint16_t listConstEnd = listConst.length - 1;
    uint16_t valueIndex = listConst.constants[listConstEnd - i];

    Cons *cons = makeCons(vm);
    cons->value = fnConstants(*protectedFn)[valueIndex];
    cons->next = protectedSeq;
    cons->metadata = protectedMeta;
    protectedSeq = (Value)cons;
  }

  popFrameRoot(vm);
  popFrameRoot(vm);

  return protectedSeq;
}

void arrayInitContents(Array *array) {
  array->header = 0;
}

Value* arrayElements(Array *array) {
  return ((void*)array) + sizeof(Array);
}

Array* makeArray(VM *vm, uint64_t size) {

  uint64_t sizeBytes = padAllocSize(sizeof(Array) + (size * sizeof(Value)));
  Array *array = alloc(vm, sizeBytes);

  arrayInitContents(array);
  array->header = makeObjectHeader(W_ARRAY_TYPE, size);

  Value *elements = ((void*)array)+ sizeof(Array);
  for (uint64_t i=0; i<size; i++) {
    elements[i] = W_NIL_VALUE;
  }

  return array;
}

void _mapEntryInitContents(MapEntry *e) {
  e->header = 0;
  e->used = false;
  e->key = nil();
  e->keyHash = 0;
  e->value = nil();
}

MapEntry* makeMapEntry(VM *vm) {

  uint64_t sizeBytes = padAllocSize(sizeof(MapEntry));
  MapEntry *entry = alloc(vm, sizeBytes);

  _mapEntryInitContents(entry);
  entry->header = makeObjectHeader(W_MAP_ENTRY_TYPE, sizeBytes);

  return entry;
}

void _mapInitContents(Map *m) {
  m->header = 0;
  m->size = 0;
  m->entries = nil();
}

Map* makeMap(VM *vm) {

  uint64_t numEntries = SYMBOL_TABLE_MIN_ENTRIES;

  Array *protectedEntries = makeArray(vm, numEntries);
  pushFrameRoot(vm, (Value*)&protectedEntries);

  for (uint64_t i=0; i<numEntries; i++) {
    arrayElements(protectedEntries)[i] = (Value)makeMapEntry(vm);
  }

  uint64_t sizeBytes = padAllocSize(sizeof(Map));
  Map *map = alloc(vm, sizeBytes);

  _mapInitContents(map);
  map->header = makeObjectHeader(W_MAP_TYPE, sizeBytes);
  map->entries = (Value)protectedEntries;

  popFrameRoot(vm); // protectedEntries

  return map;
}

/*
 * `protectedFn` is a alloc-safe pointer to the Fn for which a constant is being hydrated.
 * This is included so we so that references to already-hydrated values can be resolved by
 * constant index.
 */
Value vecHydrate(VM *vm, Fn **protectedFn, VecConstant vecConst) {

  Array *array = makeArray(vm, vecConst.length);
  Value* elements = arrayElements(array);

  for (uint16_t i = 0; i < vecConst.length; i++) {
    uint16_t valueIndex = vecConst.constants[i];
    elements[i] = fnConstants(*protectedFn)[valueIndex];
  }

  return (Value)array;
}

void putMapEntry(VM *vm, Map **protectedMap, Value key, Value insertMe);

/*
 * `protectedFn` is a alloc-safe pointer to the Fn for which a constant is being hydrated.
 * This is included so we so that references to already-hydrated values can be resolved by
 * constant index.
 */
Value mapHydrate(VM *vm, Fn **protectedFn, MapConstant mapConst) {

  Map *protectedMap = makeMap(vm);
  pushFrameRoot(vm, (Value*)&protectedMap);

  for (uint16_t i = 0; i < mapConst.length * 2; i+=2) {

    uint16_t keyIndex = mapConst.constants[i];
    Value key = fnConstants(*protectedFn)[keyIndex];

    uint16_t valueIndex = mapConst.constants[i+1];
    Value value = fnConstants(*protectedFn)[valueIndex];

    putMapEntry(vm, &protectedMap, key, value);
  }

  popFrameRoot(vm); // protectedMap

  return (Value)protectedMap;
}

// TODO: I had a thought, can we get rid of CodeUnit entirely and just replace it with FnConstant?
// TODO: I had another thought, can we get rid of the nested graph of constants and flatten it entirely?

Value hydrateConstant(VM *vm, Fn **protectedFn, Constant c) {
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
      v = listHydrate(vm, protectedFn, c.list);
      break;
    case CT_VEC:
      v = vecHydrate(vm, protectedFn, c.vec);
      break;
    case CT_MAP:
      v = mapHydrate(vm, protectedFn, c.map);
      break;
    case CT_NONE:
    default:
      explode("invalid constant: %u", c.type);
  }
  return v;
}

/*
 * Create a reader representation of a Value (an Expr).
 *
 * Some representations are approximate and cannot be round-tripped through eval, such as functions and closures.
 */
void vmPrn(VM *vm, Value result, Expr *expr) {
  ValueType resultType = valueType(result);
  exprInitContents(expr);
  Prn prn = vm->valueTypeTable.valueTypes[resultType].prn;
  prn(vm, result, expr);
}

#define ONE_KB 1024

void doPr(VM *vm, Value v) {

  Error error;
  errorInitContents(&error);

  Expr expr;
  vmPrn(vm, v, &expr);
  if (tryExprPrn(vm->outputPool, &expr, &error) != R_SUCCESS) {
    explode("failed to other prn");
  }
}

#define RAISE_MSG_LENGTH 1023

typedef struct Raised {
  wchar_t message[RAISE_MSG_LENGTH + 1];
  const char *fileName;
  uint64_t lineNumber;
  const char *functionName;
} Raised;

void exFrameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

VMException* exceptionMake(VM *vm, Raised *raised) {

  Error error;
  errorInitContents(&error);

  wchar_t msg[ERROR_MSG_LENGTH];

  VMException *exception;
  palloc(vm->outputPool, exception, sizeof(VMException), "VMException");
  exceptionInitContents(exception);

  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", raised->message);
  if (tryTextMake(vm->outputPool, msg, &exception->message, wcslen(msg), &error) != R_SUCCESS) {
    explode("make text");
  }

  uint64_t numFrames = 0;
  if (vm->current != NULL) {
    Frame_t current = vm->current;
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
  palloc(vm->outputPool, exception->frames.elements, sizeof(VMExceptionFrame) * numFrames, "VMExceptionFrame array");


  { // native frame

    VMExceptionFrame *f = &exception->frames.elements[0];
    exFrameInitContents(f);

    f->functionName.length = strlen(raised->functionName) + 1;
    palloc(vm->outputPool, f->functionName.value, f->functionName.length * sizeof(wchar_t), "wide string");
    swprintf(f->functionName.value, f->functionName.length, L"%s", raised->functionName);

    f->unknownSource = false;

    char* fileName = basename((char *) raised->fileName);
    f->fileName.length = strlen(fileName) + 1;
    palloc(vm->outputPool, f->fileName.value, f->fileName.length * sizeof(wchar_t), "wide string");
    swprintf(f->fileName.value, f->fileName.length, L"%s", fileName);

    f->lineNumber = raised->lineNumber;
  }

  if (vm->current != NULL) {
    Frame_t current = vm->current;
    for (uint64_t i = 1; i < numFrames; i++) {

      VMExceptionFrame *f = &exception->frames.elements[i];
      exFrameInitContents(f);

      if (hasFnName(current)) {
        Text text = getFnName(current);
        if (tryTextCopy(vm->outputPool, &text, &f->functionName, &error) != R_SUCCESS) {
          explode("text copy");
        }
      } else {
        wchar_t *name = L"<root>\0";
        if (tryTextMake(vm->outputPool, name, &f->functionName, wcslen(name), &error) != R_SUCCESS) {
          explode("text make");
        }
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
  }

  return exception;
}

RetVal tryExceptionPrint(Pool_t pool, VMException *e, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(pool, &b, error));

  throws(tryStringBufferAppendStr(b, e->message.value, error));

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

void raisedInitContents(Raised *r) {
  r->lineNumber = 0;
  r->functionName = NULL;
  r->fileName = NULL;
}

// TODO: exceptions should go on the heap as values
void handleRaise(VM *vm, Raised *r) {
  VMException *ex = exceptionMake(vm, r);
  setException(vm, ex);
  longjmp(vm->jumpBuf, 1);
}

#define raise(vm, str, ...) {\
  Raised r; \
  raisedInitContents(&r); \
  r.fileName = __FILE__; \
  r.lineNumber = __LINE__; \
  r.functionName = __func__; \
  \
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  \
  swprintf(r.message, ERROR_MSG_LENGTH, L"vm raised an exception: %s\n", msg); \
  handleRaise(vm, &r); \
}

/*
 * Instruction Definitions
 */

// (8), typeIndex (16) | (-> value)
void loadConstEval(VM *vm, Frame_t frame) {
  uint16_t constantIndex = readIndex(frame);
  Value constant = getConst(frame, constantIndex);
  pushOperand(frame, constant);
}

// (8), typeIndex (16) | (-> value)
void loadLocalEval(VM *vm, Frame_t frame) {
  uint16_t localIndex = readIndex(frame);
  Value v = getLocal(frame, localIndex);
  pushOperand(frame, v);
}

// (8), typeIndex  (16) | (objectref ->)
void storeLocalEval(VM *vm, Frame_t frame) {
  uint16_t localIndex = readIndex(frame);
  Value v = popOperand(frame);
  setLocal(frame, localIndex, v);
}

void invocableInitContents(Invocable *i) {
  i->ref = nil();    // the reference to the initially invoked value (could be closure or fn)
  i->fn = NULL;      // always points to the actual fn
  i->closure = NULL; // points to the closure, if there is one
}

void makeInvocable(VM *vm, Value pop, Invocable *invocable) {

  invocableInitContents(invocable);
  invocable->ref = pop;

  ValueType fnRefType = valueType(invocable->ref);
  switch (fnRefType) {
    case VT_FN: {
      invocable->fn = deref(&vm->gc, invocable->ref);
      invocable->closure = NULL;
      break;
    }
    case VT_CLOSURE: {
      invocable->closure = deref(&vm->gc, invocable->ref);
      invocable->fn = deref(&vm->gc, invocable->closure->fn);
      break;
    }
    default:
      // fail: not all values are invocable
      raise(vm, "cannot invoke this value type as a function: %s",
          getValueTypeName(vm, fnRefType));
  }
}

void protectInvocable(VM *vm, Invocable *invocable) {
  pushFrameRoot(vm, &invocable->ref);
  pushFrameRoot(vm, (Value*)&invocable->fn);
  if (invocable->closure != NULL) {
    pushFrameRoot(vm, (Value*) &invocable->closure);
  }
}

void unprotectInvocable(VM *vm, Invocable *invocable) {
  popFrameRoot(vm);
  popFrameRoot(vm);
  if (invocable->closure != NULL) {
    popFrameRoot(vm);
  }
}

void preprocessArguments(VM *vm, Frame_t parent, uint16_t numArgs, bool usesVarArgs, uint64_t numArgsSupplied) {

  if (!usesVarArgs) {
    if (numArgsSupplied != numArgs) {
      raise(vm, "required arguments not supplied, expected %u but got %" PRIu64, numArgs,
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
      raise(vm, "required arguments not supplied, expected %u or more arguments but got %" PRIu64,
                        numArgs - 1, numArgsSupplied);
    }

    Value seq = nil();
    pushFrameRoot(vm, &seq);

    // read the extra args into that sequence, push it back on the stack
    for (uint16_t i = 0; i < numVarArgs; i++) {
      Cons *cons = makeCons(vm);
      cons->value = popOperand(parent);
      cons->next = seq;
      seq = (Value)cons;
    }

    popFrameRoot(vm);
    pushOperand(parent, seq);
  }
}

void invokePopulateLocals(VM *vm, Frame_t parent, Frame_t child, Invocable *invocable, uint16_t numArgsSupplied) {

  protectInvocable(vm, invocable);

  preprocessArguments(vm, parent, invocable->fn->numArgs, invocable->fn->usesVarArgs, numArgsSupplied);

  for (uint16_t i = 0; i < invocable->fn->numArgs; i++) {
    Value arg = popOperand(parent);

    uint16_t idx = invocable->fn->numArgs - (1 + i);
    setLocal(child, idx, arg);
  }

  uint16_t numCaptures = 0;

  if (invocable->closure != NULL) {
    numCaptures = invocable->closure->numCaptures;
    uint16_t nextLocalIdx = invocable->fn->numArgs;
    for (uint16_t i=0; i<invocable->closure->numCaptures; i++) {
      setLocal(child, nextLocalIdx, closureCaptures(invocable->closure)[i]);
      nextLocalIdx = nextLocalIdx + 1;
    }
  }

  if (invocable->fn->hasName) {
    uint16_t fnLocalIndex = invocable->fn->numArgs + numCaptures;
    setLocal(child, fnLocalIndex, invocable->ref);
  }

  unprotectInvocable(vm, invocable);
}

void invokeCFn(VM *vm, Frame_t frame, Value cFn, uint16_t numArgsSupplied) {
  CFn *protectedFn = deref(&vm->gc, cFn);
  pushFrameRoot(vm, (Value*)&protectedFn);

  preprocessArguments(vm, frame, protectedFn->numArgs, protectedFn->usesVarArgs, numArgsSupplied);
  protectedFn->ptr(vm, frame);

  popFrameRoot(vm);
}

Value mapLookup(VM *vm, Map *map, Value key);

// (8)              | (objectref, args... -> ...)
void invokeDynEval(VM *vm, Frame_t frame) {
  uint16_t numArgsSupplied = readIndex(frame);
  Value pop = popOperand(frame);
  switch (valueType(pop)) {
    case VT_CFN:
      invokeCFn(vm, frame, pop, numArgsSupplied);
      break;
    case VT_KEYWORD: {
      Value key = pop;
      preprocessArguments(vm, frame, 1, false, numArgsSupplied);
      Value coll = popOperand(frame);
      Map *m = deref(&vm->gc, coll);
      Value result = mapLookup(vm, m, key);
      pushOperand(frame, result);
      break;
    }
    default: {
      Invocable invocable;
      Frame_t parent;

      makeInvocable(vm, pop, &invocable);
      frame = pushFrame(vm, (Value) invocable.fn);
      parent = getParent(frame);
      invokePopulateLocals(vm, parent, frame, &invocable, numArgsSupplied);
    }
  }
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
void invokeDynTailEval(VM *vm, Frame_t frame) {
  uint16_t numArgsSupplied = readIndex(frame);
  Value pop = popOperand(frame);
  switch (valueType(pop)) {
    case VT_CFN:
      invokeCFn(vm, frame, pop, numArgsSupplied);
      break;
    case VT_KEYWORD: {
      Value key = pop;
      preprocessArguments(vm, frame, 1, false, numArgsSupplied);
      Value coll = popOperand(frame);
      Map *m = deref(&vm->gc, coll);
      Value result = mapLookup(vm, m, key);
      pushOperand(frame, result);
      break;
    }
    default: {
      Invocable invocable;
      makeInvocable(vm, pop, &invocable);
      replaceFrame(vm, (Value) invocable.fn);
      invokePopulateLocals(vm, frame, frame, &invocable, numArgsSupplied);
    }
  }
}

// (8)              | (objectref ->)
void retEval(VM *vm, Frame_t frame) {
  Value v = popOperand(frame);
  setResult(frame, v);
}

// (8)              | (a, b -> 0 | 1)
void cmpEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  Value c = wrapBool(a == b);
  pushOperand(frame, c);
}

// (8), offset (16) | (->)
void jmpEval(VM *vm, Frame_t frame) {
  uint16_t newPc = readIndex(frame);
  setPc(frame, newPc);
}

// (8), offset (16) | (value ->)
void jmpIfEval(VM *vm, Frame_t frame) {
  Value test = popOperand(frame);
  bool truthy = isTruthy(vm, test);
  uint16_t newPc = readIndex(frame);
  if (truthy) {
    setPc(frame, newPc);
  }
}

// (8), offset (16) | (value ->)
void jmpIfNotEval(VM *vm, Frame_t frame) {
  Value test = popOperand(frame);
  bool truthy = isTruthy(vm, test);
  uint16_t newPc = readIndex(frame);
  if (!truthy) {
    setPc(frame, newPc);
  }
}

// (8)              | (a, b -> c)
void addEval(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only add integers: %s", getValueTypeName(vm, valueType(a)));
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only add integers: %s", getValueTypeName(vm, valueType(b)));
  }

  Value c = wrapUint(unwrapUint(a) + unwrapUint(b));
  pushOperand(frame, c);
}

// (8)              | (a, b -> c)
void subEval(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only subtract integers: %s", getValueTypeName(vm, valueType(a)));
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only subtract integers: %s", getValueTypeName(vm, valueType(b)));
  }

  Value c = wrapUint(unwrapUint(a) - unwrapUint(b));
  pushOperand(frame, c);
}

// (8), offset (16)  | (value ->)
void defVarEval(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  uint16_t constantIndex = readIndex(frame);
  Value varName = getConst(frame, constantIndex);

  Symbol *symbol = deref(&vm->gc, varName);

  String *name = deref(&vm->gc, symbol->name);
  printf("defining %ls\n", stringValue(name));

  symbol->valueDefined = true;
  symbol->topLevelValue = value;

  pushOperand(frame, nil());
}

// (8), offset 16  | (-> value)
void loadVarEval(VM *vm, Frame_t frame) {

  uint16_t constantIndex = readIndex(frame);
  Value value = getConst(frame, constantIndex);
  ValueType varNameType = valueType(value);

  if (varNameType != VT_SYMBOL) {
    explode("expected a symbol: %s", getValueTypeName(vm, varNameType));
  }

  Symbol *symbol = deref(&vm->gc, value);

  if (!symbol->valueDefined) {

    for (uint64_t i=0; i<vm->symbolTable.numAllocatedEntries; i++) {
      TableEntry *e = &vm->symbolTable.entries[i];
      if (e->used) {
        Symbol *s = deref(&vm->gc, e->value);
        String *name = deref(&vm->gc, s->name);
        printf("symbol-table: %ls (defined=%u)\n", stringValue(name), s->valueDefined);
      }
    }

    String *name = deref(&vm->gc, symbol->name);
    raise(vm, "no value defined for : '%ls'", stringValue(name));
  }

  pushOperand(frame, symbol->topLevelValue);
}

void closureInitContents(Closure *cl) {
  cl->header = 0;
  cl->fn = nil();
  cl->numCaptures = 0;
  cl->capturesOffset = 0;
}

// (8), offset (16) | (captures... -> value)
void loadClosureEval(VM *vm, Frame_t frame) {
  Fn *protectedFn;
  {
    uint16_t constantIndex = readIndex(frame);
    Value fnValue = getConst(frame, constantIndex);

    ValueType fnValueType = valueType(fnValue);
    if (fnValueType != VT_FN) {
      raise(vm, "cannot create a closure from this value type: %s", getValueTypeName(vm, fnValueType));
    }

    protectedFn = deref(&vm->gc, fnValue);
  }
  pushFrameRoot(vm, (Value*)&protectedFn);

  uint16_t numCaptures = readIndex(frame);

  uint64_t capturesSize = numCaptures * sizeof(Value);
  uint64_t clSize = padAllocSize(sizeof(Closure) + capturesSize);
  Closure *closure = alloc(vm, clSize);

  closureInitContents(closure);
  closure->header = makeObjectHeader(W_CLOSURE_TYPE, clSize);
  closure->fn = (Value)protectedFn;
  closure->numCaptures = numCaptures;

  closure->capturesOffset = sizeof(Closure);

  // pop captures in reverse order, same as arguments
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    Value capture = popOperand(frame);
    uint16_t idx = closure->numCaptures - (1 + i);
    closureCaptures(closure)[idx] = capture;
  }

  popFrameRoot(vm);
  pushOperand(frame, (Value)closure);
}

// (8)        | (a, b -> b, a)
void swapEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  pushOperand(frame, a);
  pushOperand(frame, b);
}

// (8)        | (jumpAddr, handler ->)
void setHandlerEval(VM *vm, Frame_t frame) {
  ExceptionHandler handler;
  handler.jumpAddress = readIndex(frame);
  handler.localIndex = readIndex(frame);
  setHandler(frame, handler);
}

// (8)        | (->)
void clearHandlerEval(VM *vm, Frame_t frame) {
  clearHandler(frame);
}

// (8),             | (x, seq -> newseq)
void consEval(VM *vm, Frame_t frame) {
  // gc may occur, so allocate the cons first
  Cons *cons = makeCons(vm);

  Value seq = popOperand(frame);
  Value x = popOperand(frame);

  ValueType seqType = valueType(seq);
  if (seqType != VT_NIL && seqType != VT_LIST) {
    raise(vm, "cannot cons onto a value of type %s", getValueTypeName(vm, seqType));
  }

  cons->value = x;
  cons->next = seq;

  pushOperand(frame, (Value)cons);
}

// (8),             | (seq -> x)
void firstEval(VM *vm, Frame_t frame) {

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
    raise(vm, "cannot get first from a value of type %s", getValueTypeName(vm, seqType));
  }

  pushOperand(frame, result);
}

// (8),             | (seq -> seq)
void restEval(VM *vm, Frame_t frame) {

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
    raise(vm, "cannot get rest from a value of type %s", getValueTypeName(vm, seqType));
  }

  pushOperand(frame, result);
}

// (8),             | (name -> nil)
void setMacroEval(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  ValueType type = valueType(value);

  if (type != VT_SYMBOL) {
    raise(vm, "only symbols can identify vars: %s", getValueTypeName(vm, type));
  }

  Symbol *s = deref(&vm->gc, value);
  String *name = deref(&vm->gc, s->name);

  if (!s->valueDefined) {

    for (uint64_t i=0; i<vm->symbolTable.numAllocatedEntries; i++) {
      TableEntry *e = &vm->symbolTable.entries[i];
      if (e->used) {
        Symbol *sym = deref(&vm->gc, e->value);
        String *symName = deref(&vm->gc, sym->name);
        printf("symbol-table: %ls (defined=%u)\n", stringValue(symName), s->valueDefined);
      }
    }

    raise(vm, "no value is defined for this var: %ls", stringValue(name));
  }

  if (!s->isMacro) {
    if (valueType(s->topLevelValue) != VT_FN) {
      raise(vm, "only vars referring to functions can be macros: %ls -> %s",
          stringValue(name), getValueTypeName(vm, valueType(s->topLevelValue)));
    }
    s->isMacro = true;
  }

  pushOperand(frame, nil());
}

// (8),             | (name -> bool)
void getMacroEval(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);

  ValueType type = valueType(value);
  if (type != VT_SYMBOL) {
    raise(vm, "only symbols can identify vars: %s", getValueTypeName(vm, type));
  }

  Symbol *s = deref(&vm->gc, value);
  Value result = wrapBool(s->isMacro);
  pushOperand(frame, result);
}

// (8),             | (name -> bool)
void gcEval(VM *vm, Frame_t frame) {
  collect(vm);
  pushOperand(frame, nil());
}

// (8),             | (value -> value)
void getTypeEval(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);
  Value typeId = wrapUint(valueType(value));
  pushOperand(frame, typeId);
}

void vmPrn(VM *vm, Value result, Expr *expr);

#define ONE_KB 1024

// (8),             | (value -> value)
void prnEval(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);

  Error error;
  errorInitContents(&error);
  Expr expr;
  vmPrn(vm, value, &expr);
  if (tryExprPrn(vm->outputPool, &expr, &error) != R_SUCCESS) {
    explode("expr prn");
  }
  printf("\n");

  pushOperand(frame, nil());
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
    table.instructions[i].eval = NULL;
  }

  // init with known instructions
  Inst instructions[]      = {
      [I_LOAD_CONST]       = { .name = "I_LOAD_CONST",      .print = printInstAndIndex,   .eval = loadConstEval },
      [I_LOAD_LOCAL]       = { .name = "I_LOAD_LOCAL",      .print = printInstAndIndex,   .eval = loadLocalEval },
      [I_STORE_LOCAL]      = { .name = "I_STORE_LOCAL",     .print = printInstAndIndex,   .eval = storeLocalEval },
      [I_INVOKE_DYN]       = { .name = "I_INVOKE_DYN",      .print = printInstAndIndex,   .eval = invokeDynEval },
      [I_INVOKE_DYN_TAIL]  = { .name = "I_INVOKE_DYN_TAIL", .print = printInstAndIndex,   .eval = invokeDynTailEval },
      [I_RET]              = { .name = "I_RET",             .print = printInst,           .eval = retEval },
      [I_CMP]              = { .name = "I_CMP",             .print = printInst,           .eval = cmpEval },
      [I_JMP]              = { .name = "I_JMP",             .print = printInstAndIndex,   .eval = jmpEval },
      [I_JMP_IF]           = { .name = "I_JMP_IF",          .print = printInstAndIndex,   .eval = jmpIfEval },
      [I_JMP_IF_NOT]       = { .name = "I_JMP_IF_NOT",      .print = printInstAndIndex,   .eval = jmpIfNotEval },
      [I_ADD]              = { .name = "I_ADD",             .print = printInst,           .eval = addEval },
      [I_SUB]              = { .name = "I_SUB",             .print = printInst,           .eval = subEval },
      [I_DEF_VAR]          = { .name = "I_DEF_VAR",         .print = printInstAndIndex,   .eval = defVarEval },
      [I_LOAD_VAR]         = { .name = "I_LOAD_VAR",        .print = printInstAndIndex,   .eval = loadVarEval },
      [I_LOAD_CLOSURE]     = { .name = "I_LOAD_CLOSURE",    .print = printInstAndIndex2x,   .eval = loadClosureEval },
      [I_SWAP]             = { .name = "I_SWAP",            .print = printInst,           .eval = swapEval },
      [I_SET_HANDLER]      = { .name = "I_SET_HANDLER",     .print = printInstAndIndex2x, .eval = setHandlerEval },
      [I_CLEAR_HANDLER]    = { .name = "I_CLEAR_HANDLER",   .print = printInst,           .eval = clearHandlerEval },

      [I_CONS]             = { .name = "I_CONS",            .print = printInst,           .eval = consEval },

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
  relocate(vm, &cons->metadata);
}

void relocateChildrenClosure(VM_t vm, void *obj) {
  Closure *closure = obj;
  relocate(vm, &closure->fn);
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    relocate(vm, &closureCaptures(closure)[i]);
  }
}

void relocateChildrenSymbol(VM_t vm, void *obj) {
  Symbol *s = obj;
  relocate(vm, &s->name);
  relocate(vm, &s->topLevelValue);
}

void relocateChildrenKeyword(VM_t vm, void *obj) {
  Keyword *k = obj;
  relocate(vm, &k->name);
}

void relocateChildrenArray(VM_t vm, void *obj) {
  Array *k = obj;
  Value *elements = arrayElements(k);
  uint64_t size = objectHeaderSize(k->header);
  for (uint64_t i=0; i<size; i++) {
    relocate(vm, &elements[i]);
  }
}

void relocateChildrenMap(VM_t vm, void *obj) {
  Map *map = obj;
  relocate(vm, &map->entries);
}

void relocateChildrenMapEntry(VM_t vm, void *obj) {
  MapEntry *mapEntry = obj;
  if (mapEntry->used) {
    relocate(vm, &mapEntry->key);
    relocate(vm, &mapEntry->value);
  }
}

void prnNil(VM_t vm, Value result, Expr *expr) {
  expr->type = N_NIL;
}

void prnInt(VM_t vm, Value result, Expr *expr) {
  expr->type = N_NUMBER;
  expr->number.value = unwrapUint(result);
}

void prnBool(VM_t vm, Value result, Expr *expr) {
  expr->type = N_BOOLEAN;
  expr->boolean.value = unwrapBool(result);
}

void prnFn(VM_t vm, Value result, Expr *expr) {
  expr->type = N_STRING;
  wchar_t function[] = L"<function>";
  expr->string.length = wcslen(function);

  Error error;
  errorInitContents(&error);
  if (tryCopyText(vm->outputPool, function, &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void prnCFn(VM_t vm, Value result, Expr *expr) {
  expr->type = N_STRING;
  wchar_t function[] = L"<c-function>";
  expr->string.length = wcslen(function);

  Error error;
  errorInitContents(&error);
  if (tryCopyText(vm->outputPool, function, &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void prnClosure(VM_t vm, Value result, Expr *expr) {
  expr->type = N_STRING;
  wchar_t function[] = L"<closure>";
  expr->string.length = wcslen(function);

  Error error;
  errorInitContents(&error);
  if (tryCopyText(vm->outputPool, function, &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void prnStr(VM_t vm, Value result, Expr *expr) {
  String *str = deref(&vm->gc, result);
  expr->type = N_STRING;
  expr->string.length = str->length;

  Error error;
  errorInitContents(&error);
  if(tryCopyText(vm->outputPool, stringValue(str), &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void prnSymbol(VM_t vm, Value result, Expr *expr) {
  Symbol *sym = deref(&vm->gc, result);
  String *str = deref(&vm->gc, sym->name);
  expr->type = N_SYMBOL;
  expr->symbol.length = str->length;

  Error error;
  errorInitContents(&error);
  if(tryCopyText(vm->outputPool, stringValue(str), &expr->symbol.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void prnKeyword(VM_t vm, Value result, Expr *expr) {
  Keyword *kw = deref(&vm->gc, result);
  String *str = deref(&vm->gc, kw->name);
  expr->type = N_KEYWORD;
  expr->keyword.length = str->length;

  Error error;
  errorInitContents(&error);
  if (tryCopyText(vm->outputPool, stringValue(str), &expr->keyword.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

bool isEmpty(Value value) {
  return valueType(value) == VT_NIL;
}

typedef struct Property {
  Keyword *key;
  Value value;
} Property;

void readProperty(VM *vm, Value *ptr, Property *p) {

  if (valueType(*ptr) != VT_LIST) {
    raise(vm, "expected property list: %s",
              getValueTypeName(vm, valueType(*ptr)));
  }

  Cons *properties = deref(&vm->gc, *ptr);

  if (valueType(properties->value) != VT_KEYWORD) {
    raise(vm, "expected keyword for property key: %s",
              getValueTypeName(vm, valueType(properties->value)));
  }

  p->key = deref(&vm->gc, properties->value);

  if (isEmpty(properties->next)) {
    String *str = deref(&vm->gc, p->key->name);
    raise(vm, "expected value for property but only found a key: %ls", stringValue(str));
  }

  properties = deref(&vm->gc, properties->next);
  p->value = properties->value;

  *ptr = properties->next;
}

void prnMetadata(VM_t vm, Value metadata, Expr *expr) {
  while (!isEmpty(metadata)) {

    Property p;
    readProperty(vm, &metadata, &p);

    String *str = deref(&vm->gc, p.key->name);

    if (wcscmp(L"line-number", stringValue(str)) == 0) {

      if (valueType(p.value) != VT_UINT) {
        raise(vm, "expected line-number property value to be an int: %s",
                          getValueTypeName(vm, valueType(p.value)));
      }

      expr->source.isSet = true;
      expr->source.lineNumber = unwrapUint(p.value);
    }
    else {
      // ignore property
    }
  }
}

void prnList(VM_t vm, Value result, Expr *expr) {
  Cons *cons = deref(&vm->gc, result);

  expr->type = N_LIST;

  prnMetadata(vm, cons->metadata, expr);

  listInitContents(&expr->list);
  Expr *elem;

  palloc(vm->outputPool, elem, sizeof(Expr), "Expr");
  exprInitContents(elem);

  vmPrn(vm, cons->value, elem);

  Error error;
  errorInitContents(&error);
  if (tryListAppend(vm->outputPool, &expr->list, elem, &error) != R_SUCCESS) {
    explode("list append");
  }

  while (valueType(cons->next) != VT_NIL) {

    if (valueType(cons->next) != VT_LIST) {
      raise(vm, "this should always be a type of VT_LIST: %s",
                getValueTypeName(vm, valueType(cons->next)));
    }

    cons = deref(&vm->gc, cons->next);

    palloc(vm->outputPool, elem, sizeof(Expr), "Expr");
    exprInitContents(elem);

    vmPrn(vm, cons->value, elem);

    if (tryListAppend(vm->outputPool, &expr->list, elem, &error) != R_SUCCESS) {
      explode("list append");
    }
  }
}

void prnMap(VM_t vm, Value result, Expr *expr) {
  Map *map = deref(&vm->gc, result);
  Array *array = deref(&vm->gc, map->entries);

  expr->type = N_MAP;
  mapInitContents(&expr->map);

  uint64_t size = objectHeaderSize(array->header);
  for (uint64_t i=0; i<size; i++) {
    Value entryRef = arrayElements(array)[i];

    if (valueType(entryRef) == VT_MAP_ENTRY) {

      MapEntry *entry = deref(&vm->gc, entryRef);
      if (entry->used) {

        Expr *keyExpr = NULL;
        palloc(vm->outputPool, keyExpr, sizeof(Expr), "Expr");
        exprInitContents(keyExpr);
        vmPrn(vm, entry->key, keyExpr);

        Expr *valueExpr = NULL;
        palloc(vm->outputPool, valueExpr, sizeof(Expr), "Expr");
        exprInitContents(valueExpr);
        vmPrn(vm, entry->value, valueExpr);

        Error e;
        errorInitContents(&e);
        if (tryMapPut(vm->outputPool, &expr->map, keyExpr, valueExpr, &e) != R_SUCCESS) {
          explode("put");
        }
      }
    }
  }
}

void prnArray(VM_t vm, Value result, Expr *expr) {

  Array *array = deref(&vm->gc, result);
  uint64_t size = objectHeaderSize(array->header);
  Value *elements = arrayElements(array);

  expr->type = N_VEC;
  vecInitContents(&expr->vec);

  Error error;
  errorInitContents(&error);

  for (uint64_t i=0; i<size; i++) {

    Expr *elem = NULL;
    palloc(vm->outputPool, elem, sizeof(Expr), "Expr");
    exprInitContents(elem);

    vmPrn(vm, elements[i], elem);

    if (tryVecAppend(vm->outputPool, &expr->vec, elem, &error) != R_SUCCESS) {
      explode("list append");
    }
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
    table.valueTypes[i].prn = NULL;
  }

  // init with known value types
  ValueTypeInfo valueTypes [] = {
      [VT_NIL]       = {.name = "nil",
                        .isTruthy = &isTruthyNo,
                        .relocateChildren = NULL,
                        .prn = &prnNil},
      [VT_UINT]      = {.name = "uint",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .prn = &prnInt},
      [VT_BOOL]      = {.name = "bool",
                        .isTruthy = &isTruthyBool,
                        .relocateChildren = NULL,
                        .prn = &prnBool},
      [VT_FN]        = {.name = "fn",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenFn,
                        .prn = &prnFn},
      [VT_STR]       = {.name = "str",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .prn = &prnStr},
      [VT_SYMBOL]    = {.name = "symbol",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenSymbol,
                        .prn = &prnSymbol},
      [VT_KEYWORD]   = {.name = "keyword",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenKeyword,
                        .prn = &prnKeyword},
      [VT_LIST]      = {.name = "list",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenList,
                        .prn = &prnList},
      [VT_CLOSURE]   = {.name = "closure",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenClosure,
                        .prn = &prnClosure},
      [VT_CFN]       = {.name = "cfn",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL,
                        .prn = &prnCFn},
      [VT_ARRAY]     = {.name = "array",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenArray,
                        .prn = &prnArray},
      [VT_MAP]       = {.name = "map",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenMap,
                        .prn = &prnMap},
      [VT_MAP_ENTRY] = {.name = "map-entry",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenMapEntry,
                        .prn = NULL},

  };
  memcpy(table.valueTypes, valueTypes, sizeof(valueTypes));
  table.numValueTypes = sizeof(valueTypes) / sizeof(valueTypes[0]);

  return table;
}

void frameEval(VM *vm) {
  uint8_t inst;
  Eval eval;

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
    eval = vm->instTable.instructions[inst].eval;
    if (eval == NULL) {
      explode("instruction unimplemented: %s (%u)", getInstName(&vm->instTable, inst), inst);
    }

    eval(vm, vm->current);
  }
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
  if (value == 0) {
    explode("invalid local value (NULL): %u", localIndex);
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
  if (frame->opStackMaxDepth == frame->opStackUsedDepth) {
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

bool hasException(VM *vm) {
  return vm->exception != NULL;
}

void setException(VM *vm, VMException *e) {
  vm->exception = e;
}

VMException* getException(VM *vm) {
  if (vm->exception == NULL) {
    explode("handler not set");
  }
  return vm->exception;
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
  for (uint16_t i = 0; i < frame->fn->numLocals; i++) {
    frame->locals[i] = nil();
  }

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
  for (uint16_t i = 0; i < fn->numLocals; i++) {
    frame->locals[i] = nil();
  }

  if (fn->maxOperandStackSize > frame->opStackMaxDepth) {

    uint16_t oldSize = frame->opStackMaxDepth;
    uint16_t newSize = fn->maxOperandStackSize;

    Value* newOpStack = stackAllocate(stack, sizeof(Value) * newSize, "opStack");
    memcpy(newOpStack, frame->opStack, oldSize * sizeof(Value));

    frame->opStackMaxDepth = newSize;
    frame->opStack = newOpStack;
  }

  frame->fnRef = newFn;
  frame->fn = fn;
  frame->result = nil();
  frame->resultAvailable = false;
  frame->pc = 0;

  return frame;
}

FrameRoot_t frameRoots(Frame_t frame) {
  return frame->roots;
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

  if (frame == NULL) {
    FrameRoot *root = stackAllocate(stack, sizeof(FrameRoot), "FrameRoot");
    root->valuePtr = rootPtr;
    root->next = vm->noFrameRoots;
    vm->noFrameRoots = root;
  }
  else {
    FrameRoot *root = stackAllocate(stack, sizeof(FrameRoot), "FrameRoot");
    root->valuePtr = rootPtr;
    root->next = frame->roots;
    frame->roots = root;
  }
}

void popFrameRoot(VM *vm) {
  Frame *frame = vm->current;
  if (frame == NULL) {
    vm->noFrameRoots = vm->noFrameRoots->next;
  }
  else {
    frame->roots = frame->roots->next;
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

void _vmEval(VM *vm, CodeUnit *codeUnit, Value *result, VMException *exception, bool *exceptionThrown) {

  FnConstant c;
  constantFnInitContents(&c);
  c.numConstants = codeUnit->numConstants;
  c.constants = codeUnit->constants;
  c.code = codeUnit->code;

  if (!setjmp(vm->jumpBuf)) {
    Value fnRef = fnHydrate(vm, &c);
    Frame_t frame = pushFrame(vm, fnRef);
    frameEval(vm);
    *result = frame->result;
    popFrame(vm);
  }
  else {
    *exceptionThrown = true;
    *exception = *vm->exception;
  }
}

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, Pool_t outputPool, VMEvalResult *result, Error *error) {

  RetVal ret;

  Value value;
  VMException exception;
  bool exceptionThrown = false;

  vm->outputPool = outputPool;

  _vmEval(vm, codeUnit, &value, &exception, &exceptionThrown);

  if (exceptionThrown) {
    result->type = RT_EXCEPTION;
    result->exception = exception;
  }
  else {
    result->type = RT_RESULT;
    vmPrn(vm, value, &result->result);
  }

  vm->outputPool = NULL;

  return R_SUCCESS;

  failure:
    return ret;
}

/*
 * builtin procedures
 */

#define ASSERT_SEQ(vm, value, ...) {\
  if (valueType(value) != VT_LIST && valueType(value) != VT_NIL) { \
    raise(vm, "expected a list type: %s", getValueTypeName(vm, valueType(value))); \
  } \
}

#define ASSERT_STR(vm, value, ...) {\
  if (valueType(value) != VT_STR) { \
    raise(vm, "expected a string type: %s", getValueTypeName(vm, valueType(value))); \
  } \
}

#define ASSERT_UINT(vm, value, ...) {\
  if (valueType(value) != VT_UINT) { \
    raise(vm, "expected a uint type: %s", getValueTypeName(vm, valueType(value))); \
  } \
}

#define ASSERT_ARRAY(vm, value, ...) {\
  if (valueType(value) != VT_ARRAY) { \
    raise(vm, "expected an array type: %s", getValueTypeName(vm, valueType(value))); \
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
void strJoinBuiltin(VM *vm, Frame_t frame) {

  Value strings = popOperand(frame);
  ASSERT_SEQ(vm, strings);
  pushFrameRoot(vm, &strings);

  uint64_t totalLength = 0;

  Value cursor = strings;
  while (valueType(cursor) != VT_NIL) {

    Cons *seq = deref(&vm->gc, cursor);

    ASSERT_STR(vm, seq->value);

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
}

void prStrBuiltinConf(VM *vm, Frame_t frame, bool readable) {
  Value value = popOperand(frame);

  Expr expr;
  exprInitContents(&expr);
  StringBuffer_t b = NULL;

  Error error;
  errorInitContents(&error);

  vmPrn(vm, value, &expr);

  if (tryStringBufferMake(vm->outputPool, &b, &error) != R_SUCCESS) {
    explode("sbmake");
  }

  if (tryExprPrnBufConf(&expr, b, readable, &error) != R_SUCCESS) {
    explode("prnbuf");
  }

  Value resultRef = stringMakeBlank(vm, stringBufferLength(b));
  String *result = deref(&vm->gc, resultRef);

  memcpy(stringValue(result), stringBufferText(b), stringBufferLength(b) * sizeof(wchar_t));

  pushOperand(frame, resultRef);
}

void prStrBuiltin(VM *vm, Frame_t frame) {
  prStrBuiltinConf(vm, frame, true);
}

void printStrBuiltin(VM *vm, Frame_t frame) {
  prStrBuiltinConf(vm, frame, false);
}

void symbolBuiltin(VM *vm, Frame_t frame) {
  Value protectedName = popOperand(frame);
  ASSERT_STR(vm, protectedName);

  pushFrameRoot(vm, &protectedName);
  Value result = symbolIntern(vm, &protectedName);
  popFrameRoot(vm);

  pushOperand(frame, result);
}

void keywordBuiltin(VM *vm, Frame_t frame) {
  Value protectedName = popOperand(frame);
  ASSERT_STR(vm, protectedName);

  pushFrameRoot(vm, &protectedName);
  Value result = keywordIntern(vm, &protectedName);
  popFrameRoot(vm);

  pushOperand(frame, result);
}

void arrayBuiltin(VM *vm, Frame_t frame) {
  Value sizeValue = popOperand(frame);
  ASSERT_UINT(vm, sizeValue);

  Array *array = makeArray(vm, unwrapUint(sizeValue));

  pushOperand(frame, (Value)array);
}

void countBuiltin(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);

  Value result = nil();
  switch (valueType(value)) {
    case VT_ARRAY: {
      Array *k = deref(&vm->gc, value);
      uint64_t size = objectHeaderSize(k->header);
      result = wrapUint(size);
      break;
    }
    case VT_STR: {
      String *s = deref(&vm->gc, value);
      result = wrapUint(s->length);
      break;
    }
    case VT_LIST: {
      uint64_t size = 0;
      Value cursor = value;
      while (valueType(cursor) != VT_NIL) {
        size++;
        Cons *cons = deref(&vm->gc, cursor);
        cursor = cons->next;
      }
      result = wrapUint(size);
      break;
    }
    case VT_MAP: {
      Map *m = deref(&vm->gc, value);
      result = wrapUint(m->size);
      break;
    }
    default:
      raise(vm, "values of this type have no length: %s", getValueTypeName(vm, valueType(value)));
  }

  pushOperand(frame, result);
}

Value mapLookup(VM *vm, Map *map, Value key);

void getBuiltin(VM *vm, Frame_t frame) {
  Value key = popOperand(frame);
  Value coll = popOperand(frame);

  Value result = nil();
  switch (valueType(coll)) {
    case VT_ARRAY: {
      ASSERT_UINT(vm, key);
      uint64_t index = unwrapUint(key);

      Array *k = deref(&vm->gc, coll);
      if (index > objectHeaderSize(k->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
      }

      Value *elements = arrayElements(k);
      result = elements[index];
      break;
    }
    case VT_MAP: {
      Map *m = deref(&vm->gc, coll);
      result = mapLookup(vm, m, key);
      break;
    }
    default:
      raise(vm, "values of this type are not indexed: %s", getValueTypeName(vm, valueType(coll)));
  }

  pushOperand(frame, result);
}

void setBuiltin(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);
  Value key = popOperand(frame);
  Value coll = popOperand(frame);

  Value result;
  switch (valueType(coll)) {
    case VT_ARRAY: {
      ASSERT_UINT(vm, key);
      uint64_t index = unwrapUint(key);
      Array *k = deref(&vm->gc, coll);
      Value *elements = arrayElements(k);
      elements[index] = value;
      result = coll;
      break;
    }
    case VT_MAP: {
      Map *protectedMap = deref(&vm->gc, coll);
      pushFrameRoot(vm, (Value*)&protectedMap);

      putMapEntry(vm, &protectedMap, key, value);
      result = (Value)protectedMap;

      popFrameRoot(vm); // protectedMap
      break;
    }
    default:
    raise(vm, "values of this type are not indexed: %s", getValueTypeName(vm, valueType(value)));
  }

  pushOperand(frame, result);
}

/*
 * hash map
 */

uint32_t hashCode(VM *vm, Value v) {
  switch (valueType(v)) {

    case VT_NIL:  return 0;
    case VT_UINT: {
      uint64_t i = unwrapUint(v);
      return (uint32_t)(i ^ (i >> 32));
    }
    case VT_BOOL: {
      if (unwrapBool(v)) {
        return 1;
      }
      else {
        return 0;
      }
    }
    case VT_STR: {
      String *s = deref(&vm->gc, v);
      return stringHash(s);
    }
    case VT_SYMBOL: {
      Symbol *sym = deref(&vm->gc, v);
      String *s = deref(&vm->gc, sym->name);
      return stringHash(s);
    }
    case VT_KEYWORD: {
      Keyword *k = deref(&vm->gc, v);
      String *s = deref(&vm->gc, k->name);
      return stringHash(s);
    }
    case VT_FN:
    case VT_LIST:
    case VT_CLOSURE:
    case VT_CFN:
    case VT_ARRAY:
    case VT_MAP:
    default:
      raise(vm, "can't hash this value: %s", getValueTypeName(vm, valueType(v)));
      return 0;
  }
}

bool equals(VM_t vm, Value this, Value that);

bool equalsStr(VM_t vm, Value this, Value that) {

  if (valueType(that) != VT_STR) {
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

bool equalsList(VM_t vm, Value this, Value that) {

  if (valueType(that) != VT_LIST) {
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
  switch (valueType(this)) {

    case VT_STR: return equalsStr(vm, this, that);
    case VT_LIST: return equalsList(vm, this, that);

    case VT_NIL:
    case VT_UINT:
    case VT_BOOL:
    case VT_SYMBOL:
    case VT_KEYWORD:
    case VT_FN:
    case VT_CLOSURE:
    case VT_CFN: {
      return this == that;
    }

    case VT_ARRAY:
    case VT_MAP:
    default:
      raise(vm, "can't compare this value: %s", getValueTypeName(vm, valueType(this)));
      return 0;
  }
}

MapEntry* findMapEntry(VM *vm, Map *map, Value key, uint32_t hash) {

  Array *array = deref(&vm->gc, map->entries);
  uint64_t numEntries = objectHeaderSize(array->header);
  Value *entries = arrayElements(array);

  uint64_t index = hash % numEntries;

  for (uint64_t i = index; i < numEntries; i++) {
    MapEntry *entry = deref(&vm->gc, entries[i]);
    if (!entry->used || equals(vm, key, entry->key)) {
      return entry;
    }
  }

  for (uint64_t i = 0; i < index; i++) {
    MapEntry *entry = deref(&vm->gc, entries[i]);
    if (!entry->used || equals(vm, key, entry->key)) {
      return entry;
    }
  }

  explode("could not find an available entry");
}

Value mapLookup(VM *vm, Map *map, Value key) {
  uint32_t hash = hashCode(vm, key);
  MapEntry *found = findMapEntry(vm, map, key, hash);
  if (found->used) {
    return found->value;
  }
  else {
    return nil();
  }
}

void _putMapEntryWithHash(VM *vm, Map *map, Value insertMe, Value key, uint32_t hash) {
  MapEntry *found = findMapEntry(vm, map, key, hash);
  if (!found->used) {
    map->size++;
    found->used = true;
  }
  found->key = key;
  found->keyHash = hash;
  found->value = insertMe;
}

void _putMapEntry(VM *vm, Map *map, Value key, Value insertMe) {
  uint32_t hash = hashCode(vm, key);
  _putMapEntryWithHash(vm, map, insertMe, key, hash);
}

void mapResize(VM *vm, Map **protectedMap, uint64_t targetEntries) {

  Array *protectedNewEntries = makeArray(vm, targetEntries);
  pushFrameRoot(vm, (Value*)&protectedNewEntries);
  for (uint64_t i=0; i<targetEntries; i++) {
    arrayElements(protectedNewEntries)[i] = (Value)makeMapEntry(vm);
  }
  popFrameRoot(vm); // protectedNewEntries

  Map *map = *protectedMap;
  Array *oldEntries = deref(&vm->gc, map->entries);
  map->size = 0;
  map->entries = (Value)protectedNewEntries;

  uint64_t numOldEntries = objectHeaderSize(oldEntries->header);
  for (uint64_t i=0; i<numOldEntries; i++) {
    MapEntry *oldEntry = deref(&vm->gc, arrayElements(oldEntries)[i]);
    if (oldEntry->used) {
      _putMapEntryWithHash(vm, *protectedMap, oldEntry->value, oldEntry->key, oldEntry->keyHash);
    }
  }
}

void putMapEntry(VM *vm, Map **protectedMap, Value key, Value insertMe) {

  _putMapEntry(vm, *protectedMap, key, insertMe);

  uint64_t numEntries;
  {
    Array *entries = deref(&vm->gc, (*protectedMap)->entries);
    numEntries = objectHeaderSize(entries->header);
  }

  float load = (float)(*protectedMap)->size / (float)numEntries;

  // resize
  if (load > SYMBOL_TABLE_MAX_LOAD || (load > SYMBOL_TABLE_MIN_ENTRIES && load < SYMBOL_TABLE_MIN_LOAD)) {

    uint64_t newAllocatedEntries;
    if (load > SYMBOL_TABLE_MAX_LOAD) {
      newAllocatedEntries = numEntries * 2;
    }
    else {
      newAllocatedEntries = numEntries / 2;
      if (newAllocatedEntries < SYMBOL_TABLE_MIN_ENTRIES) {
        newAllocatedEntries = SYMBOL_TABLE_MIN_ENTRIES;
      }
    }

    mapResize(vm, protectedMap, newAllocatedEntries);
  }
}

void hashMapBuiltin(VM *vm, Frame_t frame) {
  Value params = popOperand(frame);

  Value result;
  switch (valueType(params)) {

    case VT_NIL:
      result = (Value)makeMap(vm);
      break;

    case VT_LIST: {

      Value protectedParams = params;
      pushFrameRoot(vm, &protectedParams);

      Map *protectedMap = makeMap(vm);
      pushFrameRoot(vm, (Value*)&protectedMap);

      while (protectedParams != W_NIL_VALUE) {

        Cons *keyCons = deref(&vm->gc, protectedParams);
        if (keyCons->next == W_NIL_VALUE) {
          raise(vm, "hash-map takes an even number of parameters");
        }

        Cons *valueCons = deref(&vm->gc, keyCons->next);
        putMapEntry(vm, &protectedMap, keyCons->value, valueCons->value);
        protectedParams = valueCons->next;
      }

      popFrameRoot(vm); // protectedMap
      popFrameRoot(vm); // protectedParams

      result = (Value)protectedMap;
      break;
    }
    default:
      explode("var-args, should have been a list");
  }

  pushOperand(frame, result);
}

void vectorBuiltin(VM *vm, Frame_t frame) {

  Value params = popOperand(frame);

  Value result;
  switch (valueType(params)) {

    case VT_NIL:
      result = (Value)makeArray(vm, 0);
      break;

    case VT_LIST: {

      uint64_t length = 0;
      {
        Value seq = params;
        while (seq != W_NIL_VALUE) {
          Cons *cons = deref(&vm->gc, seq);
          length++;
          seq = cons->next;
        }
      }

      Value protectedParams = params;
      pushFrameRoot(vm, &protectedParams);

      Array *array = makeArray(vm, length);

      for (uint64_t i=0; protectedParams != W_NIL_VALUE; i++) {
        Cons *cons = deref(&vm->gc, protectedParams);
        arrayElements(array)[i] = cons->value;
        protectedParams = cons->next;
      }

      popFrameRoot(vm); // protectedParams

      result = (Value)array;
      break;
    }
    default:
    explode("var-args, should have been a list");
  }

  pushOperand(frame, result);
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

  Value protectedName = stringHydrate(vm, name, wcslen(name));
  pushFrameRoot(vm, &protectedName);

  Value protectedSymbol = symbolIntern(vm, &protectedName);
  pushFrameRoot(vm, &protectedSymbol);

  Value value = makeCFn(vm, name, numArgs, varArgs, ptr);

  Symbol *symbol = deref(&vm->gc, protectedSymbol);
  symbol->valueDefined = true;
  symbol->topLevelValue = value;

  popFrameRoot(vm);
  popFrameRoot(vm);
}

void initCFns(VM *vm) {

  defineCFn(vm, L"cons", 2, false, consEval);
  defineCFn(vm, L"first", 1, false, firstEval);
  defineCFn(vm, L"rest", 1, false, restEval);
  defineCFn(vm, L"set-macro", 1, false, setMacroEval);
  defineCFn(vm, L"get-macro", 1, false, getMacroEval);
  defineCFn(vm, L"gc", 0, false, gcEval);
  defineCFn(vm, L"get-type", 1, false, getTypeEval);
  defineCFn(vm, L"prn", 1, false, prnEval);
  defineCFn(vm, L"+", 2, false, addEval);
  defineCFn(vm, L"-", 2, false, subEval);
  defineCFn(vm, L"eq", 2, false, cmpEval);
  defineCFn(vm, L"join", 1, false, strJoinBuiltin);
  defineCFn(vm, L"pr-str", 1, false, prStrBuiltin);
  defineCFn(vm, L"print-str", 1, false, printStrBuiltin);
  defineCFn(vm, L"symbol", 1, false, symbolBuiltin);
  defineCFn(vm, L"keyword", 1, false, keywordBuiltin);
  defineCFn(vm, L"array", 1, false, arrayBuiltin);
  defineCFn(vm, L"count", 1, false, countBuiltin);
  defineCFn(vm, L"get", 2, false, getBuiltin);
  defineCFn(vm, L"set", 3, false, setBuiltin);
  defineCFn(vm, L"hash-map", 1, true, hashMapBuiltin);
  defineCFn(vm, L"vector", 1, true, vectorBuiltin);
}

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
  vm->outputPool = NULL;
  vm->exception = NULL;
  vm->noFrameRoots = NULL;
  initCFns(vm);
}

void vmFreeContents(VM *vm) {
  if (vm != NULL) {
    GCFreeContents(&vm->gc);
    // TODO: seems like we're missing a few things here
    tableFreeContents(&vm->symbolTable);
  }
}

RetVal tryVMMake(VM **ptr, VMConfig config, Error *error) {
  RetVal ret;

  tryMalloc(*ptr, sizeof(VM), "VM malloc");
  vmInitContents(*ptr, config);

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
  fnConst->numArgs = 0;
  fnConst->usesVarArgs = false;
  fnConst->numConstants = 0;
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
