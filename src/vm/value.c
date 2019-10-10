#include <string.h>
#include <stdlib.h>
#include "../errors.h"
#include "internal.h"

/*
 * value definitions
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
      return sizeof(ByteArray) + (size * sizeof(uint8_t));
    }
    case W_CHAR_ARRAY_TYPE: {
      uint64_t size = h & W_HEADER_SIZE_MASK;
      return sizeof(CharArray) + (size * sizeof(wchar_t));
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
    case W_CHAR_ARRAY_TYPE: return VT_CHAR_ARRAY;
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

wchar_t* fnName(Fn *fn) { return (void*)fn + fn->nameOffset; }
Value* fnConstants(Fn *fn) { return (void*)fn + fn->constantsOffset; }
uint8_t* fnCode(Fn *fn) { return (void*)fn + fn->codeOffset; }
wchar_t* fnSourceFileName(Fn *fn) { return (void*)fn + fn->sourceFileNameOffset; }
LineNumber* fnLineNumbers(Fn *fn) { return (void*)fn + fn->lineNumbersOffset; }
Value* closureCaptures(Closure *closure) { return (void*)closure + closure->capturesOffset; }
wchar_t* stringValue(String *x) { return (void*)x + x->valueOffset; }

uint64_t padAllocSize(uint64_t length) {
  uint16_t rem = length % 4;
  if (rem != 0) {
    return length + (4 - rem);
  }
  else {
    return length;
  }
}

/*
 * Manipulating Heap Values
 */

void cFnInitContents(CFn *fn) {
  fn->header = 0;
  fn->nameLength = 0;
  fn->nameOffset = 0;
  fn->numArgs = 0;
  fn->ptr = NULL;
  fn->usesVarArgs = false;
}

wchar_t* cFnName(CFn *fn) {
    return (void*)fn + fn->nameOffset;
}

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

void charArrayInitContents(CharArray *array) {
  array->header = 0;
}

wchar_t* charArrayElements(CharArray *array) {
  return ((void*)array) + sizeof(CharArray);
}

CharArray* makeCharArray(VM *vm, uint64_t size) {

  uint64_t sizeBytes = padAllocSize(sizeof(CharArray) + (size * sizeof(Value)));
  CharArray *array = alloc(vm, sizeBytes);

  charArrayInitContents(array);
  array->header = makeObjectHeader(W_CHAR_ARRAY_TYPE, size);

  wchar_t *elements = charArrayElements(array);
  for (uint64_t i=0; i<size; i++) {
    elements[i] = L'\0';
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


