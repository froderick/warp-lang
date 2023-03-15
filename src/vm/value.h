#ifndef WARP_LANG_VALUE_H
#define WARP_LANG_VALUE_H

#include "vm.h"
#include "frame.h"
#include "heap.h"

ObjectHeader makeObjectHeader(uint8_t objectType, uint64_t size);
uint8_t objectHeaderType(ObjectHeader h);
uint64_t objectHeaderSize(ObjectHeader h);
uint64_t objectHeaderSizeBytes(ObjectHeader h);
ValueType objectHeaderValueType(ObjectHeader header);
ValueType valueType(Value v);
Value wrapBool(bool b);
bool unwrapBool(Value v);
Value wrapUint(uint64_t i);
uint64_t unwrapUint(Value v);
Value wrapChar(wchar_t v);
wchar_t unwrapChar(Value v);

wchar_t* fnName(Fn *fn);
Value* fnConstants(Fn *fn);
uint8_t* fnCode(Fn *fn);
wchar_t* fnSourceFileName(Fn *fn);
LineNumber* fnLineNumbers(Fn *fn);
Value* closureCaptures(Closure *closure);
wchar_t* stringValue(String *x);

// instruction definitions

typedef int (*Eval) (VM_t vm, Frame_t frame);

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

typedef struct ValueTypeInfo {
  const char *name;
  bool (*isTruthy)(Value value);
  RelocateChildren relocateChildren;
} ValueTypeInfo;

typedef struct ValueTypeTable {
  uint8_t numValueTypes;
  ValueTypeInfo valueTypes[256];
} ValueTypeTable;

uint64_t padAllocSize(uint64_t length);

void fnInitContents(Fn *fn);

void stringInitContents(String *s);
Value makeString(VM_t vm, uint64_t length);
Value makeStringValue(VM_t vm, wchar_t *text, uint64_t length);

void symbolInitContents(Symbol *s);
Value symbolIntern(VM_t vm, Value *protectedName);

void keywordInitContents(Keyword *k);
Value keywordIntern(VM_t vm, Value *protectedName);

void consInitContents(Cons *c);
Cons* makeCons(VM_t vm);

void arrayInitContents(Array *array);
Value* arrayElements(Array *array);
Array* makeArray(VM_t vm, uint64_t size);

void byteArrayInitContents(ByteArray *array);
uint8_t* byteArrayElements(ByteArray *array);
ByteArray* makeByteArray(VM_t vm, uint64_t size);

void charArrayInitContents(CharArray *array);
wchar_t* charArrayElements(CharArray *array);
CharArray* makeCharArray(VM_t vm, uint64_t size);

#define W_MAP_MIN_ENTRIES 16
#define W_MAP_MIN_LOAD .40
#define W_MAP_MAX_LOAD .70
MapEntry* makeMapEntry(VM_t vm);
Map* makeMap(VM_t vm);

void recordInitContents(Record *record);
Value* recordFields(Record *record);
Record* makeRecord(VM_t vm, uint64_t numFields);

typedef int (*CFnInvoke) (VM_t vm, Frame_t frame);
void defineCFn(VM_t vm, wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr);

ValueTypeTable valueTypeTableCreate();

bool isTruthy(VM_t vm, Value value);


#endif //WARP_LANG_VALUE_H
