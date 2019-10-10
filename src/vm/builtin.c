#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>

#include "../errors.h"
#include "internal.h"

/*
 * builtin procedures, some of which are also used as instructions
 */

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

bool isSeq(Value value) {
  return valueType(value) == VT_LIST || valueType(value) == VT_NIL;
}

bool isString(Value value) {
  return valueType(value) == VT_STR;
}

bool isInt(Value value) {
  return valueType(value) == VT_UINT;
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
    case VT_NIL: {
      result = wrapUint(0);
      break;
    }
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
    case VT_CHAR_ARRAY: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      CharArray *k = deref(vm, coll);
      if (index >= objectHeaderSize(k->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      wchar_t *elements = charArrayElements(k);
      result = wrapChar(elements[index]);
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
    case VT_CHAR_ARRAY: {
      if (!isInt(key)) {
        raise(vm, "expected a number: %s", getValueTypeName(vm, valueType(key)));
        return R_ERROR;
      }
      uint64_t index = unwrapUint(key);

      CharArray *k = deref(vm, coll);
      if (index >= objectHeaderSize(k->header)) {
        raise(vm, "index out of bounds: %" PRIu64, index);
        return R_ERROR;
      }

      wchar_t *elements = charArrayElements(k);
      elements[index] = unwrapChar(value);
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

int charArrayBuiltin(VM *vm, Frame_t frame) {

  uint64_t length;
  {
    Value val = popOperand(frame);
    if (valueType(val) != VT_UINT) {
      raise(vm, "need an integer to describe the size of an array: %s", getValueTypeName(vm, valueType(val)));
      return R_ERROR;
    }
    length = unwrapUint(val);
  }

  CharArray *array = makeCharArray(vm, length);

  pushOperand(frame, (Value)array);
  return R_SUCCESS;
}

int printBuiltin(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  if (!isString(a)) {
    raise(vm, "can only print a string: %s", getValueTypeName(vm, valueType(a)));
    return R_ERROR;
  }
  String *s = deref(vm, a);
  printf("%ls", stringValue(s));
  pushOperand(frame, W_NIL_VALUE);
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

int divBuiltin(VM *vm, Frame_t frame) {
  Value b = popOperand(frame);
  Value a = popOperand(frame);

  if (valueType(a) != VT_UINT) {
    raise(vm, "can only div integers: %s", getValueTypeName(vm, valueType(a)));
    return R_ERROR;
  }
  if (valueType(b) != VT_UINT) {
    raise(vm, "can only div integers: %s", getValueTypeName(vm, valueType(b)));
    return R_ERROR;
  }

  Value c = wrapUint(unwrapUint(a) / unwrapUint(b));
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

int makeStringBuiltin(VM *vm, Frame_t frame) {

  Value protectedArray = popOperand(frame);
  ValueType type = valueType(protectedArray);
  if (type != VT_CHAR_ARRAY) {
    raise(vm, "an array of chars is required: %s", getValueTypeName(vm, type));
    return R_ERROR;
  }

  uint64_t len;
  {
    Array *a = (Array *) protectedArray;
    len = objectHeaderSize(a->header);
  }

  pushFrameRoot(vm, &protectedArray);

  String *s = (String*)makeString(vm, len);
  CharArray *a = deref(vm, protectedArray);

  wcpncpy(stringValue(s), charArrayElements(a), len);

  popFrameRoot(vm); // protectedArray

  Value result = (Value)s;
  pushOperand(frame, result);
  return R_SUCCESS;
}

void initCFns(VM *vm) {

  defineCFn(vm, L"cons", 2, false, consEval);
  defineCFn(vm, L"first", 1, false, firstEval);        // TODO: rename to car instruction
  defineCFn(vm, L"rest", 1, false, restEval);          // TODO: rename to cdr instruction
  defineCFn(vm, L"set-macro", 1, false, setMacroEval); // TODO: adopt generic metadata
  defineCFn(vm, L"get-macro", 1, false, getMacroEval); // TODO: adopt generic metadata
  defineCFn(vm, L"gc", 0, false, gcEval);
  defineCFn(vm, L"get-type", 1, false, getTypeEval);   // TODO: make instruction
  defineCFn(vm, L"+", 2, false, addEval);              // TODO: make instruction
  defineCFn(vm, L"-", 2, false, subEval);              // TODO: make instruction
  defineCFn(vm, L"*", 2, false, multBuiltin);          // TODO: make instruction
  defineCFn(vm, L"/", 2, false, divBuiltin);           // TODO: make instruction
  defineCFn(vm, L"mod", 2, false, modBuiltin);         // TODO: make instruction
  defineCFn(vm, L"eq", 2, false, cmpEval);             // TODO: make instruction
  defineCFn(vm, L"symbol", 1, false, symbolBuiltin);   // TODO: make instruction
  defineCFn(vm, L"keyword", 1, false, keywordBuiltin); // TODO: make instruction
  defineCFn(vm, L"array", 1, false, arrayBuiltin);     // TODO: move to std lib
  defineCFn(vm, L"count", 1, false, countBuiltin);     // TODO: make type-specific instructions, move to std lib
  defineCFn(vm, L"get", 2, false, getBuiltin);         // TODO: make type-specific instructions, move to std lib
  defineCFn(vm, L"set", 3, false, setBuiltin);         // TODO: make type-specific instructions, move to std lib
  defineCFn(vm, L"hash-map", 1, true, hashMapBuiltin); // TODO: move to std lib
  defineCFn(vm, L"make-vector", 1, false, makeVectorBuiltin); // TODO: make instruction
  defineCFn(vm, L"record", 2, false, recordBuiltin);   // TODO: make instruction
  defineCFn(vm, L"record-type", 1, false, recordTypeBuiltin);
  defineCFn(vm, L"uint-to-string", 1, false, uintToStringBuiltin);
  defineCFn(vm, L"throw", 1, false, throwBuiltin);
  defineCFn(vm, L"throw-value", 2, false, throwValueBuiltin);
  defineCFn(vm, L"print", 1, false, printBuiltin);
  defineCFn(vm, L"open-file", 1, false, openFileBuiltin);
  defineCFn(vm, L"read-port", 1, false, readPortBuiltin);
  defineCFn(vm, L"read-char", 1, false, readCharBuiltin);
  defineCFn(vm, L"unread-char", 2, false, unreadCharBuiltin);
  defineCFn(vm, L"close-port", 1, false, closePortBuiltin);
  defineCFn(vm, L"byte-array", 1, false, byteArrayBuiltin);
  defineCFn(vm, L"char-array", 1, false, charArrayBuiltin);
  defineCFn(vm, L"<", 2, false, ltEval); // TODO: make instruction
  defineCFn(vm, L"<=", 2, false, lteEval); // TODO: make instruction
  defineCFn(vm, L">", 2, false, gtEval);  // TODO: make instruction
  defineCFn(vm, L">=", 2, false, gteEval); // TODO: make instruction
  defineCFn(vm, L"char-to-uint", 1, false, charToUintBuiltin);
  defineCFn(vm, L"char-to-string", 1, false, charToStringBuiltin);
  defineCFn(vm, L"name", 1, false, nameBuiltin); // TODO: make type-specific instructions, move to std lib
  defineCFn(vm, L"meta", 1, false, metaBuiltin); // TODO: make the meta property on a pair optional, put in std lib?
  defineCFn(vm, L"with-meta", 2, false, withMetaBuiltin);
  defineCFn(vm, L"make-string", 1, false, makeStringBuiltin);
}
