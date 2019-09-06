#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>
#include "vm.h"
#include "../bootstrap/print.h"
#include "../errors.h"
#include <errno.h>

/*
 * VM Data Structures
 */

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
      return sizeof(Array) + (size * sizeof(Value));
    }
    case W_BYTE_ARRAY_TYPE: {
      uint64_t size = h & W_HEADER_SIZE_MASK;
      return sizeof(Array) + (size * sizeof(uint8_t));
    }
    case W_RECORD_TYPE: {
      uint64_t size = h & W_HEADER_SIZE_MASK;
      return sizeof(Record) + (size * sizeof(Value));
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
    case W_RECORD_TYPE: return VT_RECORD;
    case W_PORT_TYPE: return VT_PORT;
    case W_BYTE_ARRAY_TYPE: return VT_BYTE_ARRAY;
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
  else if (imm == W_CHARACTER_BITS) {
    return VT_CHAR;
  }
  else {
    uint8_t special = v & W_SPECIAL_MASK;
    if (special == W_NIL_BITS) {
      return VT_NIL;
    }
  }

  explode("unknown type: %" PRIu64, v);
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

Value wrapChar(wchar_t v) {
  return (v << 4u) | W_CHARACTER_BITS;
}

wchar_t unwrapChar(Value v) {
  return v >> 4u;
}

typedef struct Frame *Frame_t;
typedef struct FrameRoot *FrameRoot_t;
typedef struct FrameHandler *FrameHandler_t;

wchar_t* fnName(Fn *fn) { return (void*)fn + fn->nameOffset; }
Value* fnConstants(Fn *fn) { return (void*)fn + fn->constantsOffset; }
uint8_t* fnCode(Fn *fn) { return (void*)fn + fn->codeOffset; }
wchar_t* fnSourceFileName(Fn *fn) { return (void*)fn + fn->sourceFileNameOffset; }
LineNumber* fnLineNumbers(Fn *fn) { return (void*)fn + fn->lineNumbersOffset; }
Value* closureCaptures(Closure *closure) { return (void*)closure + closure->capturesOffset; }
wchar_t* stringValue(String *x) { return (void*)x + x->valueOffset; }

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

typedef int (*Eval) (struct VM *vm, Frame_t frame);

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

typedef struct ValueTypeInfo {
  const char *name;
  bool (*isTruthy)(Value value);
  RelocateChildren relocateChildren;
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
  FrameHandler_t noFrameHandlers;
  Frame_t current;
  Table symbolTable;
  Table keywordTable;
  Value exception;
} VM;

/*
 * Manipulating Heap Values
 */

uint64_t padAllocSize(uint64_t length);
void* alloc(VM *vm, uint64_t length);
Value tableLookup(VM *vm, Table *table, Value name);
void putEntry(VM *vm, Table *table, Value name, Value insertMe);

void cFnInitContents(CFn *fn) {
  fn->header = 0;
  fn->nameLength = 0;
  fn->nameOffset = 0;
  fn->numArgs = 0;
  fn->ptr = NULL;
  fn->usesVarArgs = false;
}

wchar_t* cFnName(CFn *fn) { return (void*)fn + fn->nameOffset; }

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

void stringInitContents(String *s) {
  s->header = 0;
  s->length = 0;
  s->valueOffset = 0;
  s->hash = 0;
}

Value makeString(VM *vm, uint64_t length) {

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

Value makeStringValue(VM *vm, wchar_t *text, uint64_t length) {
  String *str = (String*)makeString(vm, length);
  memcpy(stringValue(str), text, length * sizeof(wchar_t));
  return (Value)str;
}

void symbolInitContents(Symbol *s) {
  s->header = 0;
  s->topLevelValue = W_NIL_VALUE;
  s->valueDefined = false;
  s->name = W_NIL_VALUE;
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

void keywordInitContents(Keyword *k) {
  k->header = 0;
  k->name = W_NIL_VALUE;
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

void consInitContents(Cons *c) {
  c->header = 0;
  c->metadata = W_NIL_VALUE;
  c->value = W_NIL_VALUE;
  c->next = W_NIL_VALUE;
}

Cons* makeCons(VM *vm) {

  Cons *cons = NULL;
  uint64_t size = padAllocSize(sizeof(Cons));
  cons = alloc(vm, size);

  consInitContents(cons);
  cons->header = makeObjectHeader(W_LIST_TYPE, size);

  return cons;
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

void byteArrayInitContents(ByteArray *array) {
  array->header = 0;
}

uint8_t* byteArrayElements(ByteArray *array) {
  return ((void*)array) + sizeof(ByteArray);
}

ByteArray* makeByteArray(VM *vm, uint64_t size) {

  uint64_t sizeBytes = padAllocSize(sizeof(ByteArray) + (size * sizeof(Value)));
  ByteArray *array = alloc(vm, sizeBytes);

  byteArrayInitContents(array);
  array->header = makeObjectHeader(W_BYTE_ARRAY_TYPE, size);

  uint8_t *elements = byteArrayElements(array);
  for (uint64_t i=0; i<size; i++) {
    elements[i] = 0;
  }

  return array;
}

void _mapEntryInitContents(MapEntry *e) {
  e->header = 0;
  e->used = false;
  e->key = W_NIL_VALUE;
  e->keyHash = 0;
  e->value = W_NIL_VALUE;
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
  m->entries = W_NIL_VALUE;
}

#define W_MAP_MIN_ENTRIES 16
#define W_MAP_MIN_LOAD .40
#define W_MAP_MAX_LOAD .70

Map* makeMap(VM *vm) {

  uint64_t numEntries = W_MAP_MIN_ENTRIES;

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

void recordInitContents(Record *record) {
  record->header = 0;
  record->type = W_NIL_VALUE;
}

Value* recordFields(Record *record) {
  return ((void*)record) + sizeof(Record);
}

Record* makeRecord(VM *vm, uint64_t numFields) {

  uint64_t sizeBytes = padAllocSize(sizeof(Record) + (numFields * sizeof(Value)));
  Record *record = alloc(vm, sizeBytes);

  recordInitContents(record);
  record->header = makeObjectHeader(W_RECORD_TYPE, numFields);

  Value *fields = ((void*)record) + sizeof(Record);
  for (uint64_t i=0; i<numFields; i++) {
    fields[i] = W_NIL_VALUE;
  }

  return record;
}

typedef int (*CFnInvoke) (VM_t vm, Frame_t frame);

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

  Value protectedName = makeStringValue(vm, name, wcslen(name));
  pushFrameRoot(vm, &protectedName);

  Value protectedSymbol = symbolIntern(vm, &protectedName);
  pushFrameRoot(vm, &protectedSymbol);

  Value value = makeCFn(vm, name, numArgs, varArgs, ptr);

  Symbol *symbol = deref(vm, protectedSymbol);
  symbol->valueDefined = true;
  symbol->topLevelValue = value;

  popFrameRoot(vm);
  popFrameRoot(vm);
}

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
  e->name = W_NIL_VALUE;
  e->nameHash = 0;
  e->value = W_NIL_VALUE;
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

TableEntry* findEntry(VM *vm, Table *table, String* name, uint32_t hash) {

  uint64_t index = hash % table->numAllocatedEntries;

  for (uint64_t i = index; i < table->numAllocatedEntries; i++) {
    TableEntry *entry = &table->entries[i];

    if (!entry->used) {
      return entry;
    } else {
      String *thisName = deref(vm, entry->name);
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
      String *thisName = deref(vm, entry->name);
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

  String *s = deref(vm, name);
  uint32_t hash = stringHash(s);

  TableEntry *found = findEntry(vm, table, s, hash);

  if (found->used) {
    return found->value;
  }
  else {
    return W_NIL_VALUE;
  }
}

void _putEntryWithHash(VM *vm, Table *table, Value insertMe, Value name, uint32_t hash) {
  TableEntry *found = findEntry(vm, table, deref(vm, name), hash);
  if (!found ->used) {
    table->size++;
    found->used = true;
  }
  found->name = name;
  found->nameHash = hash;
  found->value = insertMe;
}

void _putEntry(VM *vm, Table *table, Value name, Value insertMe) {
  String *s = deref(vm, name);
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
Value* getFnRefRef(Frame_t frame);

void setFn(VM *vm, Frame_t frame, Fn *fn);

bool hasResult(Frame_t frame);
bool hasParent(Frame_t frame);
Frame_t getParent(Frame_t frame);
void setResult(Frame_t frame, Value result);
Value getResult(Frame_t frame);

typedef struct ExceptionHandler {
  uint16_t jumpAddress;
  uint16_t localIndex;
} ExceptionHandler;

bool hasFnName(Frame_t frame);
wchar_t* getFnName(Frame_t frame);

bool hasSourceTable(Frame_t frame);
bool getLineNumber(Frame_t frame, uint64_t *lineNumber);
wchar_t* getFileName(Frame_t frame);

bool hasException(VM *vm);
void setException(VM *vm, Value e);
Value getException(VM *vm);
void clearException(VM *vm);

Frame_t pushFrame(VM *vm, Value newFn);
Frame_t replaceFrame(VM *vm, Value newFn);
Frame_t popFrame(VM *vm);

FrameRoot_t frameRoots(Frame_t frame);
Value* frameRootValue(FrameRoot_t root);
FrameRoot_t frameRootNext(FrameRoot_t root);

FrameHandler_t frameHandlers(Frame_t frame);
Value* frameHandlerValue(FrameHandler_t root);
uint16_t frameHandlerJumpAddr(FrameHandler_t handler);
FrameHandler_t frameHandlerNext(FrameHandler_t root);
void pushFrameHandler(VM *vm, Value value, uint16_t jumpAddr);
void popFrameHandler(VM *vm);
void popSpecificFrameHandler(Frame_t frame);
FrameHandler_t currentHandler(Frame_t frame);
void setCurrentHandler(Frame_t frame, FrameHandler_t currentHandler);

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
void* deref(VM *vm, Value value) {
  GC *gc = &vm->gc;
  void *ptr = (void*)value;
  if (ptr < gc->currentHeap || ptr >= gc->allocPtr) {
    explode("invalid memory address: %p", ptr);
  }
  return ptr;
}

void relocate(VM *vm, Value *valuePtr) {

  Value value = *valuePtr;

  void* ptr = (void*)value;
  if (ptr >= vm->gc.currentHeap && ptr < (vm->gc.currentHeap + vm->gc.heapSize)) {
    explode("cannot relocate from current heap");
  }

  if ((value & W_PTR_MASK) != 0) {
    // only relocate heap objects
    return;
  }

  ObjectHeader *header = (ObjectHeader*)value;
  if ((*header) & W_GC_FORWARDING_BIT) {
    *valuePtr = (*header << 1u);
  }
  else {
    uint64_t size = objectHeaderSizeBytes(*header);

    void *newPtr = NULL;
    if (_alloc(&vm->gc, size, &newPtr) == R_OOM) {
      explode("out of memory, cannot allocate %" PRIu64 " bytes mid-gc", size);
    }

    memcpy(newPtr, (void*)value, size); // this must somehow be pointing to the CFn + address range

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
      relocate(vm, &(entry->name));
      relocate(vm, &(entry->value));
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

  memset(vm->gc.currentHeap, 0, vm->gc.heapSize); // TODO: this seems to clobber '+'

  // relocate exception, if present
  relocate(vm, &vm->exception);

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

  // relocate noFrameHandlers
  FrameHandler_t noFrameHandler = vm->noFrameHandlers;
  while (noFrameHandler != NULL) {
    Value *valuePtr = frameHandlerValue(noFrameHandler);
    relocate(vm, valuePtr);
    noFrameHandler = frameHandlerNext(noFrameHandler);
  }

  // relocate call stack roots
  Frame_t current = vm->current;
  if (current != NULL) {
    while (true) {

      // relocate fnRef
      {
        Value *ref = getFnRefRef(current);
        relocate(vm, ref);

        Fn *fn = deref(vm, *ref);
        setFn(vm, current, fn);
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

      FrameHandler_t handler = frameHandlers(current);
      while (handler != NULL) {
        Value *valuePtr = frameHandlerValue(handler);
        relocate(vm, valuePtr);
        handler = frameHandlerNext(handler);
      }

      if (!hasParent(current)) {
        break;
      } else {
        current = getParent(current);
      }
    }
  }

  void *scanptr = vm->gc.currentHeap;

  uint64_t count = 0;

  // relocate all the objects this object references
  while (scanptr < vm->gc.allocPtr) {
    ObjectHeader *header = scanptr;

    ValueType type = objectHeaderValueType(*header);
    uint64_t sizeBytes = objectHeaderSizeBytes(*header);

    relocateChildren(vm, type, scanptr);

    scanptr += sizeBytes;
    count++;
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

Value hydrateConstant(VM *vm, Constant c);

Value fnHydrate(VM *vm, FnConstant *fnConst) {

  Fn *fn = NULL;
  {
    uint64_t nameSize = 0;
    if (fnConst->hasName) {
      nameSize = (wcslen(fnConst->name) + 1) * sizeof(wchar_t);
    }
    uint64_t constantsSize = fnConst->numConstants * sizeof(Value);
    uint64_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
    uint64_t sourceFileNameSize = 0;
    if (fnConst->code.hasSourceTable) {
      sourceFileNameSize = (wcslen(fnConst->code.sourceTable.fileName) + 1) * sizeof(wchar_t);
    }
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
      fn->nameLength = wcslen(fnConst->name);
      size_t copySize = fn->nameLength * sizeof(wchar_t);

      memcpy(fnName(fn), fnConst->name, copySize);
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

      fn->sourceFileNameLength = wcslen(fnConst->code.sourceTable.fileName);
      size_t copySize = fn->sourceFileNameLength * sizeof(wchar_t);

      memcpy(fnSourceFileName(fn), fnConst->code.sourceTable.fileName, copySize);
      fnSourceFileName(fn)[fn->sourceFileNameLength] = L'\0';

      fn->numLineNumbers = fnConst->code.sourceTable.numLineNumbers;
      memcpy(fnLineNumbers(fn), fnConst->code.sourceTable.lineNumbers, lineNumbersSize);
    }
  }

  for (uint16_t i=0; i<fn->numConstants; i++) {
    fnConstants(fn)[i] = W_NIL_VALUE;
  }

  pushFrameRoot(vm, (Value*)&fn);
  for (uint16_t i=0; i<fn->numConstants; i++) {
    Value hydrated = hydrateConstant(vm, fnConst->constants[i]);
    fnConstants(fn)[i] = hydrated;
  }
  popFrameRoot(vm);

  return (Value)fn;
}

Value symbolHydrate(VM *vm, SymbolConstant symConst) {

  Value protectedName = makeStringValue(vm, symConst.value, symConst.length);
  pushFrameRoot(vm, &protectedName);

  Value value = symbolIntern(vm, &protectedName);

  popFrameRoot(vm);
  return value;
}

Value keywordHydrate(VM *vm, KeywordConstant kwConst) {
  Value protectedName = makeStringValue(vm, kwConst.value, kwConst.length);
  pushFrameRoot(vm, &protectedName);

  Value value = keywordIntern(vm, &protectedName);

  popFrameRoot(vm);
  return value;
}

// TODO: I had another thought, can we get rid of the nested graph of constants and flatten it entirely?

Value hydrateConstant(VM *vm, Constant c) {
  Value v;
  switch (c.type) {
    case CT_BOOL:
      v = wrapBool(c.boolean);
      break;
    case CT_INT:
      v = wrapUint(c.integer);
      break;
    case CT_CHAR:
      v = wrapChar(c.chr);
      break;
    case CT_NIL:
      v = W_NIL_VALUE;
      break;
    case CT_FN:
      v = fnHydrate(vm, &c.function);
      break;
    case CT_STR:
      v = makeStringValue(vm, c.string.value, c.string.length);
      break;
    case CT_SYMBOL:
      v = symbolHydrate(vm, c.symbol);
      break;
    case CT_KEYWORD:
      v = keywordHydrate(vm, c.keyword);
      break;
    case CT_NONE:
    default:
      explode("invalid constant: %u", c.type);
  }
  return v;
}

#define RAISE_MSG_LENGTH 1023

typedef struct Raised {
  wchar_t message[RAISE_MSG_LENGTH + 1];
  const char *fileName;
  uint64_t lineNumber;
  const char *functionName;
} Raised;

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

typedef struct ExceptionParams {
  Value *protectedMessage; // includes a string :message
  Value *protectedValue;   // includes a :value
  Raised *raised;          // includes native frame line info
} ExceptionParams;

void putMapEntry(VM *vm, Map **protectedMap, Value key, Value insertMe);

Value _exceptionMake(VM *vm, ExceptionParams p) {

  Array *protectedFrames;
  {
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

    if (p.raised != NULL) {
      // native frame
      numFrames++;
    }

    protectedFrames = makeArray(vm, numFrames);
    pushFrameRoot(vm, (Value*)&protectedFrames);
  }

  Value protectedFunctionKw = getKeyword(vm, L"function-name");
  pushFrameRoot(vm, (Value*)&protectedFunctionKw);

  Value protectedUnknownSourceKw = getKeyword(vm, L"unknown-source");
  pushFrameRoot(vm, (Value*)&protectedUnknownSourceKw);

  Value protectedFileNameKw = getKeyword(vm, L"file-name");
  pushFrameRoot(vm, (Value*)&protectedFileNameKw);

  Value protectedLineNumberKw = getKeyword(vm, L"line-number");
  pushFrameRoot(vm, (Value*)&protectedLineNumberKw);

  if (p.raised != NULL) { // native frame

    String *protectedFunctionName = (String*) makeString(vm, strlen(p.raised->functionName) + 1);
    swprintf(stringValue(protectedFunctionName), protectedFunctionName->length, L"%s", p.raised->functionName);
    pushFrameRoot(vm, (Value*)&protectedFunctionName);

    Value unknownSource = wrapBool(false);

    char* fileName = basename((char *) p.raised->fileName);
    String *protectedFileName = (String*) makeString(vm, strlen(fileName) + 1);
    swprintf(stringValue(protectedFileName), protectedFileName->length, L"%s", fileName);
    pushFrameRoot(vm, (Value*)&protectedFileName);

    Value lineNumber = wrapUint(p.raised->lineNumber);

    Map *protectedFrame = makeMap(vm);
    pushFrameRoot(vm, (Value*)&protectedFrame);

    putMapEntry(vm, &protectedFrame, protectedFunctionKw, (Value)protectedFunctionName);
    putMapEntry(vm, &protectedFrame, protectedUnknownSourceKw, unknownSource);
    putMapEntry(vm, &protectedFrame, protectedFileNameKw, (Value)protectedFileName);
    putMapEntry(vm, &protectedFrame, protectedLineNumberKw, lineNumber);

    arrayElements(protectedFrames)[0] = (Value)protectedFrame;

    popFrameRoot(vm); // protectedFrame
    popFrameRoot(vm); // protectedFileName
    popFrameRoot(vm); // protectedFunctionName
  }

  if (vm->current != NULL) {

    uint64_t start = 0;
    if (p.raised != NULL) {
      start += 1;
    }

    Frame_t current = vm->current;
    uint64_t numFrames = objectHeaderSize(protectedFrames->header);
    for (uint64_t i = start; i < numFrames; i++) {

      Map *protectedFrame = makeMap(vm);
      pushFrameRoot(vm, (Value*)&protectedFrame);

      Value protectedFnName;
      {
        wchar_t *fnName;
        if (hasFnName(current)) {
          fnName = getFnName(current);
        } else {
          fnName = L"<root>\0";
        }
        protectedFnName = makeStringValue(vm, fnName, wcslen(fnName));
        pushFrameRoot(vm, &protectedFnName);
        putMapEntry(vm, &protectedFrame, protectedFunctionKw, protectedFnName);
        popFrameRoot(vm); //protectedFnName
      }

      if (hasSourceTable(current)) {

        putMapEntry(vm, &protectedFrame, protectedUnknownSourceKw, wrapBool(false));

        {
          wchar_t *fileName = getFileName(current);
          Value protectedFileName = makeStringValue(vm, fileName, wcslen(fileName));
          pushFrameRoot(vm, &protectedFileName);
          putMapEntry(vm, &protectedFrame, protectedFileNameKw, protectedFileName);
          popFrameRoot(vm); //protectedFileName
        }

        uint64_t lineNumber;
        getLineNumber(current, &lineNumber);
        putMapEntry(vm, &protectedFrame, protectedLineNumberKw, wrapUint(lineNumber));
      }
      else {
        putMapEntry(vm, &protectedFrame, protectedUnknownSourceKw, wrapBool(true));
      }

      arrayElements(protectedFrames)[i] = (Value)protectedFrame;
      popFrameRoot(vm); // protectedFrame

      if (hasParent(current)) {
        current = getParent(current);
      }
    }
  }

  Map *protectedExn = makeMap(vm);
  pushFrameRoot(vm, (Value*)&protectedExn);

  if (p.protectedMessage != NULL) {
    putMapEntry(vm, &protectedExn, getKeyword(vm, L"message"), *p.protectedMessage);
  }
  if (p.protectedValue != NULL) {
    putMapEntry(vm, &protectedExn, getKeyword(vm, L"value"), *p.protectedValue);
  }
  putMapEntry(vm, &protectedExn, getKeyword(vm, L"frames"), (Value)protectedFrames);

  popFrameRoot(vm); // protectedExn

  popFrameRoot(vm); // protectedLineNumberKw
  popFrameRoot(vm); // protectedFileNameKw
  popFrameRoot(vm); // protectedUnknownSourceKw
  popFrameRoot(vm); // protectedFunctionKw
  popFrameRoot(vm); // protectedFrames

  return (Value)protectedExn;
}

Value exceptionMakeRaised(VM *vm, Raised *raised) {

  wchar_t msg[ERROR_MSG_LENGTH];
  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", raised->message);
  Value protectedMessage = makeStringValue(vm, msg, wcslen(msg));
  pushFrameRoot(vm, (Value *) &protectedMessage);

  ExceptionParams p;
  p.protectedMessage = &protectedMessage;
  p.protectedValue = NULL;
  p.raised = raised;

  Value v = _exceptionMake(vm, p);

  popFrameRoot(vm); // protectedMessage

  return v;
}

Value exceptionMakeKw(VM *vm, Raised *raised, wchar_t *kwName) {

  Value protectedName = makeStringValue(vm, kwName, wcslen(kwName));
  pushFrameRoot(vm, &protectedName);

  Value protectedValue = keywordIntern(vm, &protectedName);
  pushFrameRoot(vm, &protectedValue);

  ExceptionParams p;
  p.protectedMessage = &protectedName;
  p.protectedValue = &protectedValue;
  p.raised = raised;

  Value v = _exceptionMake(vm, p);

  popFrameRoot(vm); // protectedValue
  popFrameRoot(vm); // protectedName

  return v;
}

void raisedInitContents(Raised *r) {
  r->lineNumber = 0;
  r->functionName = NULL;
  r->fileName = NULL;
}

Frame_t popSpecificFrame(VM *vm, Frame_t popped);

int makeInvocable(VM *vm, Value pop, Invocable *invocable);
void invokePopulateLocals(VM *vm, Frame_t parent, Frame_t child, Invocable *invocable, uint16_t numArgsSupplied);

bool handleRaise(VM *vm) {
  Value exception = vm->exception;

  if (vm->current != NULL) { // a stack is available, look for a handler

    Frame_t f = vm->current;
    FrameHandler_t h = NULL;

    while (true) {

      FrameHandler_t current = currentHandler(f);
      if (current != NULL) { // a handler is already running, start with its immediate parent if it has one

        // discard handler frame
        f = getParent(f);

        // pop current handler from handling frame, since we already used it
        popSpecificFrameHandler(f);

        // use the parent handler within the handling frame, if any
        h = frameHandlers(f);
      }
      else { // no handler is running, look for the nearest one in the current frame
        h = frameHandlers(f);
      }

      if (h != NULL) {
        break;
      }
      else {
        if (hasParent(f)) {
          Frame_t frameToPop = f;
          f = getParent(f);
          popSpecificFrame(vm, frameToPop);
        } else {
          break;
        }
      }
    }

    if (h != NULL) { // found a handler

      {
        pushOperand(f, exception);

        Invocable invocable;
        makeInvocable(vm, *frameHandlerValue(h), &invocable);

        Frame_t handlerFrame = pushFrame(vm, (Value) invocable.fn);
        invokePopulateLocals(vm, f, handlerFrame, &invocable, 1);

        setCurrentHandler(handlerFrame, h);
      }

      uint16_t jumpAddr = frameHandlerJumpAddr(h);
      setPc(f, jumpAddr);

      clearException(vm);
      return true;
    }
  }

  // no handler, bomb out
  return false;
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
  swprintf(r.message, ERROR_MSG_LENGTH, L"vm raised an exception: %s", msg); \
  vm->exception = exceptionMakeRaised(vm, &r); \
}

#define raiseKw(vm, kwName) {\
  Raised r; \
  raisedInitContents(&r); \
  r.fileName = __FILE__; \
  r.lineNumber = __LINE__; \
  r.functionName = __func__; \
  \
  vm->exception = exceptionMakeKw(vm, &r, kwName); \
}

/*
 * Instruction Definitions
 */

// (8), typeIndex (16) | (-> value)
int loadConstEval(VM *vm, Frame_t frame) {
  uint16_t constantIndex = readIndex(frame);
  Value constant = getConst(frame, constantIndex);
  pushOperand(frame, constant);
  return R_SUCCESS;
}

// (8), typeIndex (16) | (-> value)
int loadLocalEval(VM *vm, Frame_t frame) {
  uint16_t localIndex = readIndex(frame);
  Value v = getLocal(frame, localIndex);
  pushOperand(frame, v);
  return R_SUCCESS;
}

// (8), typeIndex  (16) | (objectref ->)
int storeLocalEval(VM *vm, Frame_t frame) {
  uint16_t localIndex = readIndex(frame);
  Value v = popOperand(frame);
  setLocal(frame, localIndex, v);
  return R_SUCCESS;
}

void invocableInitContents(Invocable *i) {
  i->ref = W_NIL_VALUE;    // the reference to the initially invoked value (could be closure or fn)
  i->fn = NULL;      // always points to the actual fn
  i->closure = NULL; // points to the closure, if there is one
}

int makeInvocable(VM *vm, Value pop, Invocable *invocable) {

  invocableInitContents(invocable);
  invocable->ref = pop;

  ValueType fnRefType = valueType(invocable->ref);
  switch (fnRefType) {
    case VT_FN: {
      invocable->fn = deref(vm, invocable->ref);
      invocable->closure = NULL;
      break;
    }
    case VT_CLOSURE: {
      invocable->closure = deref(vm, invocable->ref);
      invocable->fn = deref(vm, invocable->closure->fn);
      break;
    }
    default:
      // fail: not all values are invocable
      raise(vm, "cannot invoke this value type as a function: %s",
          getValueTypeName(vm, fnRefType));
      return R_ERROR;
  }

  return R_SUCCESS;
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

int validateArguments(VM *vm, wchar_t *name, uint16_t numArgs, bool usesVarArgs, uint64_t numArgsSupplied) {

  if (!usesVarArgs) {
    if (numArgsSupplied != numArgs) {
      raise(vm, "%ls: required arguments not supplied, expected %u but got %" PRIu64, name, numArgs,
            numArgsSupplied);
      return R_ERROR;
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
      raise(vm, "%ls: required arguments not supplied, expected %u or more arguments but got %" PRIu64,
            name, numArgs - 1, numArgsSupplied);
      return R_ERROR;
    }
  }


  return R_SUCCESS;
}

void preprocessArguments(VM *vm, Frame_t parent, uint16_t numArgs, bool usesVarArgs, uint64_t numArgsSupplied) {
  if (usesVarArgs) {

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
      explode("insuficient arguments supplied");
    }

    Value seq = W_NIL_VALUE;
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

void populateArgs(Frame_t from, Frame_t to, uint16_t numArgs) {
  for (uint16_t i = 0; i < numArgs; i++) {
    Value arg = popOperand(from);

    uint16_t idx = numArgs - (1 + i);
    setLocal(to, idx, arg);
  }
}

void invokePopulateLocals(VM *vm, Frame_t parent, Frame_t child, Invocable *invocable, uint16_t numArgsSupplied) {

  protectInvocable(vm, invocable);

  preprocessArguments(vm, parent, invocable->fn->numArgs, invocable->fn->usesVarArgs, numArgsSupplied);

  populateArgs(parent, child, invocable->fn->numArgs);

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


int invokeCFn(VM *vm, Frame_t frame, Value cFn, uint16_t numArgsSupplied) {
  CFn *protectedFn = deref(vm, cFn);
  pushFrameRoot(vm, (Value*)&protectedFn);

  int error = validateArguments(vm, cFnName(protectedFn), protectedFn->numArgs, protectedFn->usesVarArgs, numArgsSupplied);
  if (error) {
    goto cleanup;
  }

  preprocessArguments(vm, frame, protectedFn->numArgs, protectedFn->usesVarArgs, numArgsSupplied);

  error = ((CFnInvoke)protectedFn->ptr)(vm, frame);
  if (error) {
    goto cleanup;
  }

  cleanup:
    popFrameRoot(vm);
    if (error) {
      return error;
    }
    else {
      return R_SUCCESS;
    }
}

// (8)              | (objectref, args... -> ...)
int invokeDynEval(VM *vm, Frame_t frame) {
  uint16_t numArgsSupplied = readIndex(frame);
  Value pop = popOperand(frame);
  switch (valueType(pop)) {
    case VT_CFN: {
      int error = invokeCFn(vm, frame, pop, numArgsSupplied);
      if (error) {
        return error;
      }
      break;
    }
    case VT_KEYWORD: {
      Value key = pop;

      int error = validateArguments(vm, L"keyword-get", 1, false, numArgsSupplied);
      if (error) {
        return error;
      }

      preprocessArguments(vm, frame, 1, false, numArgsSupplied);
      Value coll = popOperand(frame);
      Map *m = deref(vm, coll);
      Value result = mapLookup(vm, m, key);
      pushOperand(frame, result);
      break;
    }
    default: {

      Invocable invocable;
      Frame_t parent;

      int error = makeInvocable(vm, pop, &invocable);
      if (error) {
        return error;
      }

      error = validateArguments(vm, fnName(invocable.fn), invocable.fn->numArgs, invocable.fn->usesVarArgs, numArgsSupplied);
      if (error) {
        return error;
      }

      frame = pushFrame(vm, (Value) invocable.fn);
      parent = getParent(frame);
      invokePopulateLocals(vm, parent, frame, &invocable, numArgsSupplied);
    }
  }

  return R_SUCCESS;
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
int invokeDynTailEval(VM *vm, Frame_t frame) {
  uint16_t numArgsSupplied = readIndex(frame);
  Value pop = popOperand(frame);
  switch (valueType(pop)) {
    case VT_CFN: {
      int error = invokeCFn(vm, frame, pop, numArgsSupplied);
      if (error) {
        return error;
      }
      break;
    }
    case VT_KEYWORD: {
      Value key = pop;

      int error = validateArguments(vm, L"keyword-get", 1, false, numArgsSupplied);
      if (error) {
        return error;
      }

      preprocessArguments(vm, frame, 1, false, numArgsSupplied);
      Value coll = popOperand(frame);
      Map *m = deref(vm, coll);
      Value result = mapLookup(vm, m, key);
      pushOperand(frame, result);
      break;
    }
    default: {
      Invocable invocable;

      int error = makeInvocable(vm, pop, &invocable);
      if (error) {
        return error;
      }

      error = validateArguments(vm, fnName(invocable.fn), invocable.fn->numArgs, invocable.fn->usesVarArgs, numArgsSupplied);
      if (error) {
        return error;
      }

      replaceFrame(vm, (Value) invocable.fn);
      invokePopulateLocals(vm, frame, frame, &invocable, numArgsSupplied);
    }
  }

  return R_SUCCESS;
}

// (8)              | (args... -> ...)
int invokeDynTailEvalRecurse(VM *vm, Frame_t frame) {
  uint16_t numArgs = readIndex(frame);
  populateArgs(frame, frame, numArgs);
  setPc(frame, 0);
  return R_SUCCESS;
}

// (8)              | (objectref ->)
int retEval(VM *vm, Frame_t frame) {
  Value v = popOperand(frame);
  setResult(frame, v);
  return R_SUCCESS;
}

// (8)              | (a, b -> 0 | 1)
int cmpEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  Value c = wrapBool(a == b);
  pushOperand(frame, c);
  return R_SUCCESS;
}

// (8)              | (a, b -> bool)
int ltEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  Value c = wrapBool(b < a);
  pushOperand(frame, c);
  return R_SUCCESS;
}

int lteEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  Value c = wrapBool(b <= a);
  pushOperand(frame, c);
  return R_SUCCESS;
}

int gtEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  Value c = wrapBool(b > a);
  pushOperand(frame, c);
  return R_SUCCESS;
}

int gteEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  Value c = wrapBool(b >= a);
  pushOperand(frame, c);
  return R_SUCCESS;
}

// (8), offset (16) | (->)
int jmpEval(VM *vm, Frame_t frame) {
  uint16_t newPc = readIndex(frame);
  setPc(frame, newPc);
  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
int jmpIfEval(VM *vm, Frame_t frame) {
  Value test = popOperand(frame);
  bool truthy = isTruthy(vm, test);
  uint16_t newPc = readIndex(frame);
  if (truthy) {
    setPc(frame, newPc);
  }
  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
int jmpIfNotEval(VM *vm, Frame_t frame) {
  Value test = popOperand(frame);
  bool truthy = isTruthy(vm, test);
  uint16_t newPc = readIndex(frame);
  if (!truthy) {
    setPc(frame, newPc);
  }
  return R_SUCCESS;
}

// (8)              | (a, b -> c)
int addEval(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only add integers: %s", getValueTypeName(vm, valueType(a)));
    return R_ERROR;
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only add integers: %s", getValueTypeName(vm, valueType(b)));
    return R_ERROR;
  }

  Value c = wrapUint(unwrapUint(a) + unwrapUint(b));
  pushOperand(frame, c);
  return R_SUCCESS;
}

// (8)              | (a, b -> c)
int subEval(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only subtract integers: %s", getValueTypeName(vm, valueType(a)));
    return R_ERROR;
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only subtract integers: %s", getValueTypeName(vm, valueType(b)));
    return R_ERROR;
  }

  Value c = wrapUint(unwrapUint(a) - unwrapUint(b));
  pushOperand(frame, c);
  return R_SUCCESS;
}

// (8), offset (16)  | (value ->)
int defVarEval(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  uint16_t constantIndex = readIndex(frame);
  Value varName = getConst(frame, constantIndex);

  Symbol *symbol = deref(vm, varName);

  String *name = deref(vm, symbol->name);
  printf("defining %ls\n", stringValue(name));

  symbol->valueDefined = true;
  symbol->topLevelValue = value;

  pushOperand(frame, W_NIL_VALUE);
  return R_SUCCESS;
}

// (8), offset 16  | (-> value)
int loadVarEval(VM *vm, Frame_t frame) {

  uint16_t constantIndex = readIndex(frame);
  Value value = getConst(frame, constantIndex);
  ValueType varNameType = valueType(value);

  if (varNameType != VT_SYMBOL) {
    explode("expected a symbol: %s", getValueTypeName(vm, varNameType));
  }

  Symbol *symbol = deref(vm, value);

  if (!symbol->valueDefined) {

    for (uint64_t i=0; i<vm->symbolTable.numAllocatedEntries; i++) {
      TableEntry *e = &vm->symbolTable.entries[i];
      if (e->used) {
        Symbol *s = deref(vm, e->value);
        String *name = deref(vm, s->name);
        printf("symbol-table: %ls (defined=%u)\n", stringValue(name), s->valueDefined);
      }
    }

    String *name = deref(vm, symbol->name);
    raise(vm, "no value defined for : '%ls'", stringValue(name));
    return R_ERROR;
  }

  pushOperand(frame, symbol->topLevelValue);
  return R_SUCCESS;
}

void closureInitContents(Closure *cl) {
  cl->header = 0;
  cl->fn = W_NIL_VALUE;
  cl->numCaptures = 0;
  cl->capturesOffset = 0;
}

// (8), offset (16) | (captures... -> value)
int loadClosureEval(VM *vm, Frame_t frame) {
  Fn *protectedFn;
  {
    uint16_t constantIndex = readIndex(frame);
    Value fnValue = getConst(frame, constantIndex);

    ValueType fnValueType = valueType(fnValue);
    if (fnValueType != VT_FN) {
      raise(vm, "cannot create a closure from this value type: %s", getValueTypeName(vm, fnValueType));
      return R_ERROR;
    }

    protectedFn = deref(vm, fnValue);
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
  return R_SUCCESS;
}

// (8)        | (a, b -> b, a)
int swapEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  pushOperand(frame, a);
  pushOperand(frame, b);
  return R_SUCCESS;
}

// (8)        | (a, b -> b, a)
int dropEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  return R_SUCCESS;
}

// (8)        | (jumpAddr, handler ->)
int setHandlerEval(VM *vm, Frame_t frame) {
  uint16_t jumpIndex = readIndex(frame);

  Value handler = popOperand(frame);
  ValueType handlerType = valueType(handler);
  if (handlerType != VT_FN) {
    raise(vm, "handlers must be of type fn: %s", getValueTypeName(vm, handlerType));
    return R_ERROR;
  }

  pushFrameHandler(vm, handler, jumpIndex);
  return R_SUCCESS;
}

// (8)        | (->)
int clearHandlerEval(VM *vm, Frame_t frame) {
  popFrameHandler(vm);
  return R_SUCCESS;
}

// (8),             | (x, seq -> newseq)
int consEval(VM *vm, Frame_t frame) {
  // gc may occur, so allocate the cons first
  Cons *cons = makeCons(vm);

  Value seq = popOperand(frame);
  Value x = popOperand(frame);

  ValueType seqType = valueType(seq);
  if (seqType != VT_NIL && seqType != VT_LIST) {
    raise(vm, "cannot cons onto a value of type %s", getValueTypeName(vm, seqType));
    return R_ERROR;
  }

  cons->value = x;
  cons->next = seq;

  pushOperand(frame, (Value)cons);

  return R_SUCCESS;
}

// (8),             | (seq -> x)
int firstEval(VM *vm, Frame_t frame) {

  Value seq = popOperand(frame);
  ValueType seqType = valueType(seq);

  Value result;
  if (seqType == VT_NIL) {
    result = W_NIL_VALUE;
  }
  else if (seqType == VT_LIST) {
    Cons *cons = deref(vm, seq);
    result = cons->value;
  }
  else {
    raise(vm, "cannot get first from a value of type %s", getValueTypeName(vm, seqType));
    return R_ERROR;
  }

  pushOperand(frame, result);

  return R_SUCCESS;
}

// (8),             | (seq -> seq)
int restEval(VM *vm, Frame_t frame) {

  Value seq = popOperand(frame);
  ValueType seqType = valueType(seq);

  Value result;
  if (seqType == VT_NIL) {
    result = W_NIL_VALUE;
  }
  else if (seqType == VT_LIST) {
    Cons *cons = deref(vm, seq);
    result = cons->next;
  }
  else {
    raise(vm, "cannot get rest from a value of type %s", getValueTypeName(vm, seqType));
    return R_ERROR;
  }

  pushOperand(frame, result);

  return R_SUCCESS;
}

// (8),             | (name -> nil)
int setMacroEval(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  ValueType type = valueType(value);

  if (type != VT_SYMBOL) {
    raise(vm, "only symbols can identify vars: %s", getValueTypeName(vm, type));
    return R_ERROR;
  }

  Symbol *s = deref(vm, value);
  String *name = deref(vm, s->name);

  if (!s->valueDefined) {

    for (uint64_t i=0; i<vm->symbolTable.numAllocatedEntries; i++) {
      TableEntry *e = &vm->symbolTable.entries[i];
      if (e->used) {
        Symbol *sym = deref(vm, e->value);
        String *symName = deref(vm, sym->name);
        printf("symbol-table: %ls (defined=%u)\n", stringValue(symName), s->valueDefined);
      }
    }

    raise(vm, "no value is defined for this var: %ls", stringValue(name));
    return R_ERROR;
  }

  if (!s->isMacro) {
    if (valueType(s->topLevelValue) != VT_FN) {
      raise(vm, "only vars referring to functions can be macros: %ls -> %s",
          stringValue(name), getValueTypeName(vm, valueType(s->topLevelValue)));
      return R_ERROR;
    }
    s->isMacro = true;
  }

  pushOperand(frame, W_NIL_VALUE);
  return R_SUCCESS;
}

// (8),             | (name -> bool)
int getMacroEval(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);

  ValueType type = valueType(value);
  if (type != VT_SYMBOL) {
    raise(vm, "only symbols can identify vars: %s", getValueTypeName(vm, type));
    return R_ERROR;
  }

  Symbol *s = deref(vm, value);
  Value result = wrapBool(s->isMacro);
  pushOperand(frame, result);

  return R_SUCCESS;
}

// (8),             | (name -> bool)
int gcEval(VM *vm, Frame_t frame) {
  collect(vm);
  pushOperand(frame, W_NIL_VALUE);
  return R_SUCCESS;
}

// (8),             | (value -> value)
int getTypeEval(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);
  ValueType type = valueType(value);

  // TODO: this is very slow, we should hold references to these symbols directly

  wchar_t *name = NULL;

  switch (type) {
    case VT_NIL:
      name = L"nil";
      break;
    case VT_UINT:
      name = L"uint";
      break;
    case VT_BOOL:
      name = L"bool";
      break;
    case VT_FN:
      name = L"fn";
      break;
    case VT_STR:
      name = L"string";
      break;
    case VT_SYMBOL:
      name = L"symbol";
      break;
    case VT_KEYWORD:
      name = L"keyword";
      break;
    case VT_LIST:
      name = L"list";
      break;
    case VT_CLOSURE:
      name = L"closure";
      break;
    case VT_CFN:
      name = L"cfn";
      break;
    case VT_ARRAY:
      name = L"array";
      break;
    case VT_MAP:
      name = L"map";
      break;
    case VT_MAP_ENTRY:
      name = L"map-entry";
      break;
    case VT_RECORD:
      name = L"record";
      break;
    case VT_PORT:
      name = L"port";
      break;
    case VT_BYTE_ARRAY:
      name = L"byte-array";
      break;
    case VT_CHAR:
      name = L"char";
      break;
    default:
      explode("unhandled type: %s", getValueTypeName(vm, type));
  }

  Value protectedName = makeStringValue(vm, name, wcslen(name));
  pushFrameRoot(vm, &protectedName);
  Value result = symbolIntern(vm, &protectedName);
  popFrameRoot(vm);

  pushOperand(frame, result);
  return R_SUCCESS;
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
      [I_INVOKE_DYN_TAIL_RECURSE]  = { .name = "I_INVOKE_DYN_TAIL_RECURSE", .print = printInstAndIndex,   .eval = invokeDynTailEvalRecurse},
      [I_RET]              = { .name = "I_RET",             .print = printInst,           .eval = retEval },
      [I_CMP]              = { .name = "I_CMP",             .print = printInst,           .eval = cmpEval },
      [I_JMP]              = { .name = "I_JMP",             .print = printInstAndIndex,   .eval = jmpEval },
      [I_JMP_IF]           = { .name = "I_JMP_IF",          .print = printInstAndIndex,   .eval = jmpIfEval },
      [I_JMP_IF_NOT]       = { .name = "I_JMP_IF_NOT",      .print = printInstAndIndex,   .eval = jmpIfNotEval },
      [I_ADD]              = { .name = "I_ADD",             .print = printInst,           .eval = addEval },
      [I_SUB]              = { .name = "I_SUB",             .print = printInst,           .eval = subEval },
      [I_DEF_VAR]          = { .name = "I_DEF_VAR",         .print = printInstAndIndex,   .eval = defVarEval },
      [I_LOAD_VAR]         = { .name = "I_LOAD_VAR",        .print = printInstAndIndex,   .eval = loadVarEval },
      [I_LOAD_CLOSURE]     = { .name = "I_LOAD_CLOSURE",    .print = printInstAndIndex2x, .eval = loadClosureEval },
      [I_SWAP]             = { .name = "I_SWAP",            .print = printInst,           .eval = swapEval },
      [I_PUSH_HANDLER]     = { .name = "I_PUSH_HANDLER",     .print = printInstAndIndex,   .eval = setHandlerEval },
      [I_POP_HANDLER]      = { .name = "I_POP_HANDLER",   .print = printInst,           .eval = clearHandlerEval },
      [I_CONS]             = { .name = "I_CONS",            .print = printInst,           .eval = consEval },
      [I_DROP]             = { .name = "I_DROP",            .print = printInst,           .eval = dropEval},

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

  {
    String *name = (String*)s->name;
    if (wcscmp(stringValue(name), L"+") == 0) {
      // found it

      Value value = s->topLevelValue;
      if (value != W_NIL_VALUE) {
        ObjectHeader *h = (ObjectHeader *) value;
        if (!(*h & W_GC_FORWARDING_BIT)) {
          if (objectHeaderSizeBytes(*h) > 1000000) {
            printf("relocateChildren pre gotcha");
          }
        }
      }
    }
  }

  relocate(vm, &s->name);
  relocate(vm, &s->topLevelValue);

  {
    String *name = (String*)s->name;
    if (wcscmp(stringValue(name), L"+") == 0) {
      // found it

      Value value = s->topLevelValue;
      if (value != W_NIL_VALUE) {
        ObjectHeader *h = (ObjectHeader *) value;
        if (!(*h & W_GC_FORWARDING_BIT)) {
          if (objectHeaderSizeBytes(*h) > 1000000) {
            printf("relocateChildren post gotcha");
          }
        }
      }
    }
  }
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

void relocateChildrenRecord(VM_t vm, void *obj) {
  Record *record = obj;
  relocate(vm, &record->type);
  Value *fields = recordFields(record);
  uint64_t size = objectHeaderSize(record->header);
  for (uint64_t i=0; i<size; i++) {
    relocate(vm, &fields[i]);
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
  }

  // init with known value types
  ValueTypeInfo valueTypes [] = {
      [VT_NIL]       = {.name = "nil",
                        .isTruthy = &isTruthyNo,
                        .relocateChildren = NULL},
      [VT_UINT]      = {.name = "uint",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL},
      [VT_BOOL]      = {.name = "bool",
                        .isTruthy = &isTruthyBool,
                        .relocateChildren = NULL},
      [VT_FN]        = {.name = "fn",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenFn},
      [VT_STR]       = {.name = "str",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL},
      [VT_SYMBOL]    = {.name = "symbol",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenSymbol},
      [VT_KEYWORD]   = {.name = "keyword",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenKeyword},
      [VT_LIST]      = {.name = "list",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenList},
      [VT_CLOSURE]   = {.name = "closure",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenClosure},
      [VT_CFN]       = {.name = "cfn",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL},
      [VT_ARRAY]     = {.name = "array",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenArray},
      [VT_MAP]       = {.name = "map",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenMap},
      [VT_MAP_ENTRY] = {.name = "map-entry",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenMapEntry},
      [VT_RECORD]    = {.name = "record",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = &relocateChildrenRecord},
      [VT_PORT]      = {.name = "port",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL},
      [VT_BYTE_ARRAY]= {.name = "byte-array",
                        .isTruthy = &isTruthyYes,
                        .relocateChildren = NULL},

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

    int raised = eval(vm, vm->current);
    if (raised) {
      bool handled = handleRaise(vm);
      if (handled) { // exception thrown and handled, keep evaluating
        continue;
      }
      else {
        break;
      }
    }
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

typedef struct FrameHandler FrameHandler;

typedef struct FrameHandler {
  Value value;
  uint16_t jumpAddr;
  FrameHandler *next;
} FrameHandler;

void frameHandlerInitContents(FrameHandler *handler) {
  handler->value = W_NIL_VALUE;
  handler->jumpAddr = 0;
  handler->next = NULL;
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
  FrameHandler *handlers;

  Value result;
  bool resultAvailable;
  uint16_t pc;

  /*
   * Only non-null if a frame is executing a handler. References a handler in the parent frame.
   *
   * This reference is used o handle the case where an exception is thrown through the top
   * of a handler-function. This allows handleRaise() to pick the next highest handler within
   * the parent function to handle this problem, if it exists. Otherwise handleRaise() crawls
   * up the stack as normal.
   */
  FrameHandler_t currentHandler;
} Frame;

void handlerInitContents(ExceptionHandler *h) {
  h->localIndex = 0;
  h->jumpAddress = 0;
}

void frameInitContents(Frame *frame) {
  frame->parent = NULL;
  frame->fnRef = W_NIL_VALUE;
  frame->fn = NULL;
  frame->locals = NULL;
  frame->opStack = NULL;
  frame->resultAvailable = 0;
  frame->result = W_NIL_VALUE;
  frame->pc = 0;

  frame->opStackUsedDepth = 0;
  frame->opStackMaxDepth = 0;
  frame->opStack = NULL;

  frame->roots = NULL;
  frame->handlers = NULL;

  frame->currentHandler = NULL;
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

Value* getFnRefRef(Frame_t frame) {
  return &(frame->fnRef);
}

void setFnRef(VM *vm, Frame *frame, Value value) {
  frame->fnRef = value;
  frame->fn = deref(vm, value);
}

void setFn(VM *vm, Frame *frame, Fn *fn) {
  frame->fn = fn;
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

bool hasFnName(Frame *frame) {
  return frame->fn->hasName;
}

wchar_t* getFnName(Frame_t frame) {
  if (!frame->fn->hasName) {
    explode("no fn name found");
  }
  return fnName(frame->fn);
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

wchar_t* getFileName(Frame_t frame) {
  if (frame->fn->hasSourceTable) {
    return fnSourceFileName(frame->fn);
  }
  return NULL;
}

bool hasException(VM *vm) {
  return vm->exception != W_NIL_VALUE;
}

void setException(VM *vm, Value value) {
  vm->exception = value;
}

void clearException(VM *vm) {
  vm->exception = W_NIL_VALUE;
}

Value getException(VM *vm) {
  if (vm->exception == W_NIL_VALUE) {
    explode("handler not set");
  }
  return vm->exception;
}

Frame_t pushFrame(VM *vm, Value newFn) {

  Fn *fn = deref(vm, newFn);

  Stack *stack = &vm->stack;
  Frame *parent = vm->current;

  Frame *frame = stackAllocate(stack, sizeof(Frame), "ExecFrame");
  frameInitContents(frame);

  frame->parent = parent;
  frame->fnRef = newFn;
  frame->fn = fn;

  frame->locals = stackAllocate(stack, sizeof(Value) * frame->fn->numLocals, "locals");
  for (uint16_t i = 0; i < frame->fn->numLocals; i++) {
    frame->locals[i] = W_NIL_VALUE;
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

Frame* popSpecificFrame(VM *vm, Frame_t popped) {
  if (vm->current == NULL) {
    explode("no frames on stack");
  }
  vm->current = vm->current->parent;
  stackFree(&vm->stack, popped);
  return vm->current;
}

Frame* replaceFrame(VM *vm, Value newFn) {
  Fn *fn = deref(vm, newFn);

  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  if (fn->numLocals > frame->fn->numLocals) {
    frame->locals = stackAllocate(stack, sizeof(Value) * fn->numLocals, "locals");
  }
  for (uint16_t i = 0; i < fn->numLocals; i++) {
    frame->locals[i] = W_NIL_VALUE;
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
  frame->result = W_NIL_VALUE;
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

FrameHandler_t frameHandlers(Frame_t frame) {
  return frame->handlers;
}

Value* frameHandlerValue(FrameHandler_t handler) {
  return &handler->value;
}

uint16_t frameHandlerJumpAddr(FrameHandler_t handler) {
  return handler->jumpAddr;
}

FrameHandler_t frameHandlerNext(FrameHandler_t handler) {
  return handler->next;
}

void pushFrameHandler(VM *vm, Value value, uint16_t jumpAddr) {
  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  FrameHandler *handler = stackAllocate(stack, sizeof(FrameHandler), "FrameHandler");
  frameHandlerInitContents(handler);
  handler->value = value;
  handler->jumpAddr = jumpAddr;

  if (frame == NULL) {
    handler->next = vm->noFrameHandlers;
    vm->noFrameHandlers = handler;
  }
  else {
    handler->next = frame->handlers;
    frame->handlers = handler;
  }
}

void popFrameHandler(VM *vm) {
  Frame *frame = vm->current;
  if (frame == NULL) {
    vm->noFrameHandlers = vm->noFrameHandlers->next;
  }
  else {
    frame->handlers = frame->handlers->next;
  }
}

void popSpecificFrameHandler(Frame_t frame) {
  frame->handlers = frame->handlers->next;
}

FrameHandler_t currentHandler(Frame_t frame) {
  return frame->currentHandler;
}

void setCurrentHandler(Frame_t frame, FrameHandler_t currentHandler) {
  frame->currentHandler = currentHandler;
}

//bool hasHandler(VM *vm) {
//  Frame *frame = vm->current;
//  if (frame == NULL) {
//    return vm->noFrameHandlers != NULL;
//  }
//  else {
//    return vm->current->handlers != NULL;
//    frame->handlers = frame->handlers->next;
//  }
//  if (frame == NULL) {
//}
//
//Value getHandler(Frame_t frame) {
//  if (frame->handlers == NULL) {
//    explode("handler not set");
//  }
//  return frame->handlers->value;
//}

/*
 * The top level frame could just be a regular frame
 * - that has an extra local, which points to a function
 * - which happens to be the function created from the root expression and allocated on the heap
 *
 * This would remove all special cases in stack stuff, except for how that initial function gets hydrated, allocated, and bound
 *
 */

VMEvalResult vmEval(VM *vm, CodeUnit *codeUnit) {

  // set up
  {
    FnConstant c;
    constantFnInitContents(&c);
    c.numConstants = codeUnit->numConstants;
    c.constants = codeUnit->constants;
    c.code = codeUnit->code;

    Value fnRef = fnHydrate(vm, &c);
    pushFrame(vm, fnRef);
  }

  VMEvalResult result;

  frameEval(vm);
  if (!hasException(vm)) {
    result.type = RT_RESULT;
    result.value = vm->current->result;
  }
  else {
    result.type = RT_EXCEPTION;
    result.value = getException(vm);
    clearException(vm);
  }

  // clean up
  popFrame(vm);

  if (vm->current != NULL) {
    explode("dangling frames found!\n");
  }

  return result;
}

/*
 * builtin procedures
 */

bool isSeq(Value value) {
  return valueType(value) == VT_LIST || valueType(value) == VT_NIL;
}

bool isString(Value value) {
  return valueType(value) == VT_STR;
}

bool isInt(Value value) {
  return valueType(value) == VT_UINT;
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
int strJoinBuiltin(VM *vm, Frame_t frame) {

  Value strings = popOperand(frame);
  if (!isSeq(strings)) {
    raise(vm, "expected a list type: %s", getValueTypeName(vm, valueType(strings)));
    return R_ERROR;
  }
  pushFrameRoot(vm, &strings);

  uint64_t totalLength = 0;

  Value cursor = strings;
  while (valueType(cursor) != VT_NIL) {

    Cons *seq = deref(vm, cursor);

    if (!isString(seq->value)) {
      raise(vm, "expected a string type: %s", getValueTypeName(vm, valueType(seq->value)));
      goto cleanup;
    }

    String *string = deref(vm, seq->value);
    totalLength += string->length;

    cursor = seq->next;
  }

  Value resultRef = makeString(vm, totalLength);
  String *result = deref(vm, resultRef);

  uint64_t totalSizeWritten = 0;

  cursor = strings;
  while (valueType(cursor) != VT_NIL) {
    Cons *seq = deref(vm, cursor);

    String *string = deref(vm, seq->value);

    wchar_t *writePtr = (void*)result + result->valueOffset + totalSizeWritten;
    size_t textSize = string->length * sizeof(wchar_t);
    memcpy(writePtr, stringValue(string), textSize);
    totalSizeWritten += textSize;

    cursor = seq->next;
  }

  popFrameRoot(vm);
  pushOperand(frame, resultRef);
  return R_SUCCESS;

  cleanup:
    popFrameRoot(vm);
    return R_ERROR;

}

int symbolBuiltin(VM *vm, Frame_t frame) {
  Value protectedName = popOperand(frame);

  if (!isString(protectedName)) {
    raise(vm, "expected a string type: %s", getValueTypeName(vm, valueType(protectedName)));
    return R_ERROR;
  }

  pushFrameRoot(vm, &protectedName);
  Value result = symbolIntern(vm, &protectedName);
  popFrameRoot(vm);

  pushOperand(frame, result);

  return R_SUCCESS;
}

int keywordBuiltin(VM *vm, Frame_t frame) {
  Value protectedName = popOperand(frame);
  ValueType type = valueType(protectedName);

  switch (type) {
    case VT_STR:
      // do nothing
      break;
    case VT_SYMBOL: {
      Symbol *s = deref(vm, protectedName);
      protectedName = s->name;
      break;
    }
    default:
      raise(vm, "expected a string type: %s", getValueTypeName(vm, valueType(protectedName)));
      return R_ERROR;
  }

  pushFrameRoot(vm, &protectedName);
  Value result = keywordIntern(vm, &protectedName);
  popFrameRoot(vm);

  pushOperand(frame, result);
  return R_SUCCESS;
}

int arrayBuiltin(VM *vm, Frame_t frame) {
  Value sizeValue = popOperand(frame);
  if (!isInt(sizeValue)) {
    raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(sizeValue)));
    return R_ERROR;
  }

  Array *array = makeArray(vm, unwrapUint(sizeValue));

  pushOperand(frame, (Value)array);
  return R_SUCCESS;
}

int countBuiltin(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);

  Value result = W_NIL_VALUE;
  switch (valueType(value)) {
    case VT_ARRAY: {
      Array *k = deref(vm, value);
      uint64_t size = objectHeaderSize(k->header);
      result = wrapUint(size);
      break;
    }
    case VT_STR: {
      String *s = deref(vm, value);
      result = wrapUint(s->length);
      break;
    }
    case VT_LIST: {
      uint64_t size = 0;
      Value cursor = value;
      while (valueType(cursor) != VT_NIL) {
        size++;
        Cons *cons = deref(vm, cursor);
        cursor = cons->next;
      }
      result = wrapUint(size);
      break;
    }
    case VT_MAP: {
      Map *m = deref(vm, value);
      result = wrapUint(m->size);
      break;
    }
    default:
      raise(vm, "values of this type have no length: %s", getValueTypeName(vm, valueType(value)));
      return R_ERROR;
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

Value mapLookup(VM *vm, Map *map, Value key);

int getBuiltin(VM *vm, Frame_t frame) {
  Value key = popOperand(frame);
  Value coll = popOperand(frame);

  Value result;
  switch (valueType(coll)) {
    case VT_ARRAY: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      Array *k = deref(vm, coll);
      if (index >= objectHeaderSize(k->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      Value *elements = arrayElements(k);
      result = elements[index];
      break;
    }
    case VT_STR: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      String *k = deref(vm, coll);
      if (index >= k->length) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      wchar_t *chars = stringValue(k);
      result = wrapChar(chars[index]);
      break;
    }
    case VT_MAP: {
      Map *m = deref(vm, coll);
      result = mapLookup(vm, m, key);
      break;
    }
    case VT_RECORD: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      Record *record = deref(vm, coll);
      if (index >= objectHeaderSize(record->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      Value *fields = recordFields(record);
      result = fields[index];
      break;
    }
    default:
      raise(vm, "values of this type are not indexed: %s", getValueTypeName(vm, valueType(coll)));
      return R_ERROR;
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

int setBuiltin(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);
  Value key = popOperand(frame);
  Value coll = popOperand(frame);

  Value result;
  switch (valueType(coll)) {
    case VT_ARRAY: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      Array *k = deref(vm, coll);
      if (index >= objectHeaderSize(k->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      Value *elements = arrayElements(k);
      elements[index] = value;
      result = coll;
      break;
    }
    case VT_MAP: {
      Map *protectedMap = deref(vm, coll);
      pushFrameRoot(vm, (Value*)&protectedMap);

      putMapEntry(vm, &protectedMap, key, value);
      result = (Value)protectedMap;

      popFrameRoot(vm); // protectedMap
      break;
    }
    case VT_RECORD: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      Record *record = deref(vm, coll);
      if (index >= objectHeaderSize(record->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      Value *fields = recordFields(record);
      fields[index] = value;
      result = coll;
      break;
    }
    case VT_STR: {
      raise(vm, "strings are not mutable");
      return R_ERROR;
    }
    default:
      raise(vm, "values of this type are not indexed: %s", getValueTypeName(vm, valueType(value)));
      return R_ERROR;
  }

  pushOperand(frame, result);
  return R_SUCCESS;
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
      String *s = deref(vm, v);
      return stringHash(s);
    }
    case VT_SYMBOL: {
      Symbol *sym = deref(vm, v);
      String *s = deref(vm, sym->name);
      return stringHash(s);
    }
    case VT_KEYWORD: {
      Keyword *k = deref(vm, v);
      String *s = deref(vm, k->name);
      return stringHash(s);
    }
    case VT_FN:
    case VT_LIST:
    case VT_CLOSURE:
    case VT_CFN:
    case VT_ARRAY:
    case VT_MAP:
    default:
      explode("can't hash this value: %s", getValueTypeName(vm, valueType(v)));
  }
}

bool equals(VM_t vm, Value this, Value that);

bool equalsStr(VM_t vm, Value this, Value that) {

  if (valueType(that) != VT_STR) {
    return false;
  }
  else {

    String *a = deref(vm, this);
    String *b = deref(vm, that);

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
    while (valueType(this) != VT_NIL) {

      Cons *a = deref(vm, this);
      Cons *b = deref(vm, that);

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

  Array *array = deref(vm, map->entries);
  uint64_t numEntries = objectHeaderSize(array->header);
  Value *entries = arrayElements(array);

  uint64_t index = hash % numEntries;

  for (uint64_t i = index; i < numEntries; i++) {
    MapEntry *entry = deref(vm, entries[i]);
    if (!entry->used || equals(vm, key, entry->key)) {
      return entry;
    }
  }

  for (uint64_t i = 0; i < index; i++) {
    MapEntry *entry = deref(vm, entries[i]);
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
    return W_NIL_VALUE;
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
  Array *oldEntries = deref(vm, map->entries);
  map->size = 0;
  map->entries = (Value)protectedNewEntries;

  uint64_t numOldEntries = objectHeaderSize(oldEntries->header);
  for (uint64_t i=0; i<numOldEntries; i++) {
    MapEntry *oldEntry = deref(vm, arrayElements(oldEntries)[i]);
    if (oldEntry->used) {
      _putMapEntryWithHash(vm, *protectedMap, oldEntry->value, oldEntry->key, oldEntry->keyHash);
    }
  }
}

void putMapEntry(VM *vm, Map **protectedMap, Value key, Value insertMe) {

  _putMapEntry(vm, *protectedMap, key, insertMe);

  uint64_t numEntries;
  {
    Array *entries = deref(vm, (*protectedMap)->entries);
    numEntries = objectHeaderSize(entries->header);
  }

  float load = (float)(*protectedMap)->size / (float)numEntries;

  // resize
  if (load > W_MAP_MAX_LOAD || (load > W_MAP_MIN_ENTRIES && load < W_MAP_MIN_LOAD)) {

    uint64_t newAllocatedEntries;
    if (load > W_MAP_MAX_LOAD) {
      newAllocatedEntries = numEntries * 2;
    }
    else {
      newAllocatedEntries = numEntries / 2;
      if (newAllocatedEntries < W_MAP_MIN_ENTRIES) {
        newAllocatedEntries = W_MAP_MIN_ENTRIES;
      }
    }

    mapResize(vm, protectedMap, newAllocatedEntries);
  }
}

int hashMapBuiltin(VM *vm, Frame_t frame) {
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

        Cons *keyCons = deref(vm, protectedParams);
        if (keyCons->next == W_NIL_VALUE) {
          raise(vm, "hash-map takes an even number of parameters");
          return R_ERROR;
        }

        Cons *valueCons = deref(vm, keyCons->next);
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
  return R_SUCCESS;
}

int listBuiltin(VM *vm, Frame_t frame) {

  Value params = popOperand(frame);
  if (!isSeq(params)) {
    raise(vm, "expected a list: %s", getValueTypeName(vm, valueType(params)));
    return R_ERROR;
  }

  pushOperand(frame, params);
  return R_SUCCESS;
}

int makeVectorBuiltin(VM *vm, Frame_t frame) {

  Value p = popOperand(frame);

  if (!isInt(p)) {
    raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(p)));
    return R_ERROR;
  }

  uint64_t length = unwrapUint(p);
  Array *array = makeArray(vm, length);

  pushOperand(frame, (Value)array);
  return R_SUCCESS;
}

int vectorBuiltin(VM *vm, Frame_t frame) {

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
          Cons *cons = deref(vm, seq);
          length++;
          seq = cons->next;
        }
      }

      Value protectedParams = params;
      pushFrameRoot(vm, &protectedParams);

      Array *array = makeArray(vm, length);

      for (uint64_t i=0; protectedParams != W_NIL_VALUE; i++) {
        Cons *cons = deref(vm, protectedParams);
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
  return R_SUCCESS;
}

int recordBuiltin(VM *vm, Frame_t frame) {

  uint16_t numFields;
  {
    Value fieldsValue = popOperand(frame);
    if (valueType(fieldsValue) != VT_UINT) {
      explode("numFields must be a uint: %s", getValueTypeName(vm, valueType(fieldsValue)));
    }
    numFields = unwrapUint(fieldsValue);
  }

  Value protectedRecordType= popOperand(frame);
  pushFrameRoot(vm, &protectedRecordType);

  Record *record = makeRecord(vm, numFields);
  record->type = protectedRecordType;

  popFrameRoot(vm); // protectedSymbol
  pushOperand(frame, (Value)record);
  return R_SUCCESS;
}

int recordTypeBuiltin(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  ValueType type = valueType(value);

  if (type != VT_RECORD) {
    raise(vm, "requires a record, got: %s", getValueTypeName(vm, type));
    return R_ERROR;
  }

  Record *record = deref(vm, value);
  pushOperand(frame, record->type);
  return R_SUCCESS;
}

int uintToStringBuiltin(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  ValueType type = valueType(value);

  Value result;
  switch (type) {
    case VT_UINT: {
      wchar_t s[30];
      uint64_t number = unwrapUint(value);
      swprintf(s, 20, L"%" PRIu64, number);
      result = makeStringValue(vm, s, wcslen(s));
      break;
    }
    default:
      explode("unhandled type: %s", getValueTypeName(vm, type));
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

int charToStringBuiltin(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  ValueType type = valueType(value);

  Value result;
  switch (type) {
    case VT_CHAR: {
      wchar_t s[1];
      wchar_t ch = unwrapChar(value);
      swprintf(s, 20, L"%lc", ch);
      result = makeStringValue(vm, s, wcslen(s));
      break;
    }
    default:
    explode("unhandled type: %s", getValueTypeName(vm, type));
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

int throwBuiltin(VM *vm, Frame_t frame) {

  Value msg = popOperand(frame);
  if (valueType(msg) != VT_STR) {
    ValueType vt = valueType(msg);
    raise(vm, "can only throw a string message: %s", getValueTypeName(vm, vt));
    return R_ERROR;
  }

  ExceptionParams p;
  p.protectedMessage = &msg;
  p.protectedValue = NULL;
  p.raised = NULL;

  pushFrameRoot(vm, p.protectedMessage);

  Value exception = _exceptionMake(vm, p);
  setException(vm, exception);

  popFrameRoot(vm); // protectedMessage
  return R_ERROR;
}

int throwValueBuiltin(VM *vm, Frame_t frame) {

  Value v = popOperand(frame);

  Value msg = popOperand(frame);
  if (valueType(msg) != VT_STR) {
    ValueType vt = valueType(msg);
    raise(vm, "can only throw a string message: %s", getValueTypeName(vm, vt));
    return R_ERROR;
  }

  ExceptionParams p;
  p.protectedMessage = &msg;
  p.protectedValue = &v;
  p.raised = NULL;

  pushFrameRoot(vm, p.protectedMessage);
  pushFrameRoot(vm, p.protectedValue);

  Value exception = _exceptionMake(vm, p);
  setException(vm, exception);

  popFrameRoot(vm); // protectedValue
  popFrameRoot(vm); // protectedMessage
  return R_ERROR;
}

void portInitContents(Port *port) {
  port->header = 0;
  port->type = PT_NONE;
  port->fileDesc = 0;
  port->closed = false;
}

Value makeFilePort(VM *vm, FILE *f) {

  uint64_t size = padAllocSize(sizeof(Port));
  Port *port = alloc(vm, size);

  portInitContents(port);
  port->header = makeObjectHeader(W_PORT_TYPE, size);
  port->type = PT_FILE;
  port->fileDesc = f;

  return (Value)port;
}

int openFileBuiltin(VM *vm, Frame_t frame) {

  Value val = popOperand(frame);
  if (valueType(val) != VT_STR) {
    raise(vm, "need a string to describe a file path: %s", getValueTypeName(vm, valueType(val)));
    return R_ERROR;
  }
  String *path = deref(vm, val);

  FILE *f = NULL;
  {
    size_t bufSize = (sizeof(wchar_t) * path->length) + 1;
    char *fileName = malloc(bufSize);
    size_t ret = wcstombs(fileName, stringValue(path), bufSize);

    if (ret < 0) {
      free(fileName);
      raise(vm, "failed to convert path string to multibyte char: %s", strerror(errno));
      return R_ERROR;
    }

    f = fopen(fileName, "r");
    if (f == NULL) {
      raise(vm, "failed to open file: %s", strerror(errno));
      return R_ERROR;
    }

    free(fileName);
  }

  Value port = makeFilePort(vm, f);
  pushOperand(frame, port);
  return R_SUCCESS;
}

int readPortBuiltin(VM *vm, Frame_t frame) {

  Value val = popOperand(frame);
  if (valueType(val) != VT_PORT) {
    raise(vm, "takes a port: %s", getValueTypeName(vm, valueType(val)));
    return R_ERROR;
  }

  Port *port = deref(vm, val);
  if (port->closed) {
    raise(vm, "port is closed");
    return R_ERROR;
  }

  uint8_t read[1];
  size_t result = fread(read, sizeof(uint8_t), 1, port->fileDesc);

  if (result > 0) {
    pushOperand(frame, wrapUint(read[0]));
    return R_SUCCESS;
  }
  else if (feof(port->fileDesc)) {
    raiseKw(vm, L"eof");
    return R_ERROR;
  }
  else if (ferror(port->fileDesc)) {
    raise(vm, "error: %s", strerror(errno));
    return R_ERROR;
  }
  else {
    explode("unhandled outcome");
  }
}

int readCharBuiltin(VM *vm, Frame_t frame) {

  Value val = popOperand(frame);
  if (valueType(val) != VT_PORT) {
    raise(vm, "takes a port: %s", getValueTypeName(vm, valueType(val)));
    return R_ERROR;
  }

  Port *port = deref(vm, val);
  if (port->closed) {
    raise(vm, "port is closed");
    return R_ERROR;
  }

  wchar_t ch = fgetwc(port->fileDesc);
  if (ch != WEOF) {
    pushOperand(frame, wrapChar(ch));
    return R_SUCCESS;
  }
  if (feof(port->fileDesc)) {
    raiseKw(vm, L"eof");
    return R_ERROR;
  }
  else if (ferror(port->fileDesc)) {
    raise(vm, "error: %s", strerror(errno));
    return R_ERROR;
  }
  else {
    explode("unhandled outcome");
  }
}

int unreadCharBuiltin(VM *vm, Frame_t frame) {

  Value charVal = popOperand(frame);
  if (valueType(charVal) != VT_CHAR) {
    raise(vm, "takes a character: %s", getValueTypeName(vm, valueType(charVal)));
    return R_ERROR;
  }
  wchar_t ch = unwrapChar(charVal);

  Value portVal = popOperand(frame);
  if (valueType(portVal) != VT_PORT) {
    raise(vm, "takes a port: %s", getValueTypeName(vm, valueType(portVal)));
    return R_ERROR;
  }

  Port *port = deref(vm, portVal);
  if (port->closed) {
    raise(vm, "port is closed");
    return R_ERROR;
  }

  wint_t result = ungetwc(ch, port->fileDesc);
  if (result == WEOF) {
    raise(vm, "failed to push character back onto stream");
  }

  pushOperand(frame, W_NIL_VALUE);
  return R_SUCCESS;
}

/*
 * (def f (open-file "/Users/ddcmhenry/dev/funtastic/branches/warp-lang/foo.txt"))
 * (read-port f)
 * (close-port f)
 */

int closePortBuiltin(VM *vm, Frame_t frame) {

  Value val = popOperand(frame);
  if (valueType(val) != VT_PORT) {
    raise(vm, "takes a port: %s", getValueTypeName(vm, valueType(val)));
    return R_ERROR;
  }

  Port *port = deref(vm, val);
  if (!port->closed) {
    fclose(port->fileDesc);
    port->fileDesc = NULL;
    port->closed = true;
  }

  pushOperand(frame, (Value)port);
  return R_SUCCESS;
}

int byteArrayBuiltin(VM *vm, Frame_t frame) {

  uint64_t length;
  {
    Value val = popOperand(frame);
    if (valueType(val) != VT_UINT) {
      raise(vm, "need an integer to describe the size of an array: %s", getValueTypeName(vm, valueType(val)));
      return R_ERROR;
    }
    length = unwrapUint(val);
  }

  ByteArray *array = makeByteArray(vm, length);

  pushOperand(frame, (Value)array);
  return R_SUCCESS;
}

int symbolEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  pushOperand(frame, wrapBool(valueType(a) == VT_SYMBOL));
  return R_SUCCESS;
}

// (8),             | (value -> value)
int charToUintBuiltin(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);
  ValueType type = valueType(value);

  if (type != VT_CHAR) {
    raise(vm, "cannot convert this type to a uint: %s", getValueTypeName(vm, type));
    return R_ERROR;
  }

  wchar_t ch = unwrapChar(value);
  pushOperand(frame, wrapUint(ch));
  return R_SUCCESS;
}

int multBuiltin(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only multiply integers: %s", getValueTypeName(vm, valueType(a)));
    return R_ERROR;
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only multiply integers: %s", getValueTypeName(vm, valueType(b)));
    return R_ERROR;
  }

  Value c = wrapUint(unwrapUint(a) * unwrapUint(b));
  pushOperand(frame, c);
  return R_SUCCESS;
}

int modBuiltin(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only mod integers: %s", getValueTypeName(vm, valueType(a)));
    return R_ERROR;
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only mod integers: %s", getValueTypeName(vm, valueType(b)));
    return R_ERROR;
  }

  Value c = wrapUint(unwrapUint(a) % unwrapUint(b));
  pushOperand(frame, c);
  return R_SUCCESS;
}

// (8),             | (value -> value)
int nameBuiltin(VM *vm, Frame_t frame) {
  Value value = popOperand(frame);
  ValueType type = valueType(value);

  Value result;
  switch (type) {
    case VT_SYMBOL: {
      Symbol *symbol = deref(vm, value);
      result = symbol->name;
      break;
    }
    case VT_KEYWORD: {
      Keyword *keyword = deref(vm, value);
      result = keyword->name;
      break;
    }
    default:
      raise(vm, "only symbols and keywords have names: %s", getValueTypeName(vm, type));
      return R_ERROR;
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

int withMetaBuiltin(VM *vm, Frame_t frame) {
  Value meta = popOperand(frame);
  Value obj = popOperand(frame);

  ValueType type = valueType(obj);
  Value result;
  switch (type) {
    case VT_NIL: {
      result = W_NIL_VALUE;
      break;
    }
    case VT_LIST: {
      Cons *cons = deref(vm, obj);
      cons->metadata = meta;
      result = obj;
      break;
    }
    default:
      raise(vm, "only lists can have metadata: %s", getValueTypeName(vm, type));
      return R_ERROR;
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

int metaBuiltin(VM *vm, Frame_t frame) {
  Value obj = popOperand(frame);

  ValueType type = valueType(obj);
  Value result;
  switch (type) {
    case VT_LIST: {
      Cons *cons = deref(vm, obj);
      result = cons->metadata;
      break;
    }
    default: raise(vm, "only lists can have metadata: %s", getValueTypeName(vm, type));
      return R_ERROR;
  }

  pushOperand(frame, result);
  return R_SUCCESS;
}

void initCFns(VM *vm) {

  defineCFn(vm, L"cons", 2, false, consEval);
  defineCFn(vm, L"first", 1, false, firstEval);
  defineCFn(vm, L"rest", 1, false, restEval);
  defineCFn(vm, L"set-macro", 1, false, setMacroEval);
  defineCFn(vm, L"get-macro", 1, false, getMacroEval);
  defineCFn(vm, L"gc", 0, false, gcEval);
  defineCFn(vm, L"get-type", 1, false, getTypeEval);
  defineCFn(vm, L"+", 2, false, addEval);
  defineCFn(vm, L"-", 2, false, subEval);
  defineCFn(vm, L"*", 2, false, multBuiltin);
  defineCFn(vm, L"mod", 2, false, modBuiltin);
  defineCFn(vm, L"eq", 2, false, cmpEval);
  defineCFn(vm, L"join", 1, false, strJoinBuiltin);
  defineCFn(vm, L"symbol", 1, false, symbolBuiltin);
  defineCFn(vm, L"keyword", 1, false, keywordBuiltin);
  defineCFn(vm, L"array", 1, false, arrayBuiltin);
  defineCFn(vm, L"count", 1, false, countBuiltin);
  defineCFn(vm, L"get", 2, false, getBuiltin);
  defineCFn(vm, L"set", 3, false, setBuiltin);
  defineCFn(vm, L"hash-map", 1, true, hashMapBuiltin);
  defineCFn(vm, L"list", 1, true, listBuiltin);
  defineCFn(vm, L"make-vector", 1, false, makeVectorBuiltin);
  defineCFn(vm, L"vector", 1, true, vectorBuiltin);
  defineCFn(vm, L"record", 2, false, recordBuiltin);
  defineCFn(vm, L"record-type", 1, false, recordTypeBuiltin);
  defineCFn(vm, L"uint-to-string", 1, false, uintToStringBuiltin);
  defineCFn(vm, L"throw", 1, false, throwBuiltin);
  defineCFn(vm, L"throw-value", 2, false, throwValueBuiltin);
  defineCFn(vm, L"open-file", 1, false, openFileBuiltin);
  defineCFn(vm, L"read-port", 1, false, readPortBuiltin);
  defineCFn(vm, L"read-char", 1, false, readCharBuiltin);
  defineCFn(vm, L"unread-char", 2, false, unreadCharBuiltin);
  defineCFn(vm, L"close-port", 1, false, closePortBuiltin);
  defineCFn(vm, L"byte-array", 1, false, byteArrayBuiltin);
  defineCFn(vm, L"<", 2, false, ltEval);
  defineCFn(vm, L"<=", 2, false, lteEval);
  defineCFn(vm, L">", 2, false, gtEval);
  defineCFn(vm, L">=", 2, false, gteEval);
  defineCFn(vm, L"symbol?", 1, false, symbolEval);
  defineCFn(vm, L"char-to-uint", 1, false, charToUintBuiltin);
  defineCFn(vm, L"char-to-string", 1, false, charToStringBuiltin);
  defineCFn(vm, L"name", 1, false, nameBuiltin);
  defineCFn(vm, L"meta", 1, false, metaBuiltin);
  defineCFn(vm, L"with-meta", 2, false, withMetaBuiltin);
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
  vm->exception = W_NIL_VALUE;
  vm->noFrameRoots = NULL;
  vm->noFrameHandlers = NULL;
  initCFns(vm);
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


