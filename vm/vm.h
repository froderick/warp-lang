#ifndef WARP_LANG_VM_H
#define WARP_LANG_VM_H

#include "../bytecode.h"

/*
 * VM runtime data structures
 */

typedef uint64_t Value;

#define W_UINT_MASK      0x01u
#define W_UINT_BITS      0x01u
#define W_PTR_MASK       0x03u
#define W_IMMEDIATE_MASK 0x0fu
#define W_CHARACTER_BITS 0x0au
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
  VT_MAP_ENTRY,
  VT_RECORD,
  VT_PORT,
  VT_BYTE_ARRAY,
  VT_CHAR,
} ValueType;

#define W_GC_FORWARDING_BIT      0x8000000000000000L   /* header contains forwarding pointer */
#define W_BYTEBLOCK_BIT          0x4000000000000000L   /* block contains bytes instead of slots */
#define W_SPECIALBLOCK_BIT       0x2000000000000000L   /* 1st item is a non-value */
#define W_8ALIGN_BIT             0x1000000000000000L   /* data is aligned to 8-byte boundary */
#define W_HEADER_TYPE_BITS       0x0f00000000000000L
#define W_HEADER_SIZE_MASK       0x00ffffffffffffffL

#define W_FN_TYPE         0x0u
#define W_STR_TYPE        0x1u
#define W_SYMBOL_TYPE     0x2u
#define W_KEYWORD_TYPE    0x3u
#define W_LIST_TYPE       0x4u
#define W_CLOSURE_TYPE    0x5u
#define W_CFN_TYPE        0x6u
#define W_ARRAY_TYPE      0x7u
#define W_MAP_TYPE        0x8u
#define W_MAP_ENTRY_TYPE  0x9u
#define W_RECORD_TYPE     0xau
#define W_PORT_TYPE       0xbu
#define W_BYTE_ARRAY_TYPE 0xcu

/*
 * This is the first field inside all heap objects. It must come first so that the GC can
 * scan through the heap, for which it needs to determine object sizes and object types.
 */
typedef uint64_t ObjectHeader;

uint8_t objectHeaderType(ObjectHeader h);
uint64_t objectHeaderSize(ObjectHeader h);
ValueType objectHeaderValueType(ObjectHeader header);
ValueType valueType(Value v);

Value wrapBool(bool b);
bool unwrapBool(Value v);
Value wrapUint(uint64_t i);
uint64_t unwrapUint(Value v);
Value wrapChar(wchar_t v);
wchar_t unwrapChar(Value v);

typedef struct CFn {
  ObjectHeader header;

  uint64_t nameLength;
  size_t nameOffset;
  void* ptr;
  uint16_t numArgs;
  bool usesVarArgs;
} CFn;

wchar_t* cFnName(CFn *fn);

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

wchar_t* fnName(Fn *fn);
Value* fnConstants(Fn *fn);
uint8_t* fnCode(Fn *fn);
wchar_t* fnSourceFileName(Fn *fn);
LineNumber* fnLineNumbers(Fn *fn);

typedef struct Closure {
  ObjectHeader header;

  Value fn;
  uint16_t numCaptures;
  size_t capturesOffset;
} Closure;

Value* closureCaptures(Closure *closure);

typedef struct String {
  ObjectHeader header;

  uint64_t length;
  size_t valueOffset;
  uint32_t hash;
} String;

wchar_t* stringValue(String *x);

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

typedef struct Record {
  ObjectHeader header;
  Value symbol;
} Record;

typedef enum PortType {
  PT_NONE,
  PT_FILE
} PortType;

typedef struct Port {
  ObjectHeader header;
  uint8_t type;
  bool closed;
  union {
    FILE *fileDesc;
  };
} Port;

typedef struct ByteArray {
  ObjectHeader header;
} ByteArray;

Value* arrayElements(Array *array);

uint8_t* byteArrayElements(ByteArray *array);

/*
 * VM code eval contract
 */

typedef struct VMConfig {
  bool gcOnAlloc;
} VMConfig;

void vmConfigInitContents(VMConfig *config);

typedef struct VM *VM_t;
VM_t vmMake(VMConfig config);
void vmFreeContents(VM_t vm);
void vmFree(VM_t vm);

Value mapLookup(VM_t vm, Map *map, Value key);
Value getKeyword(VM_t vm, wchar_t *text);

typedef enum VMEvalResultType {
  RT_RESULT,
  RT_EXCEPTION
} VMEvalResultType;

typedef struct VMEvalResult {
  VMEvalResultType type;
  Value value;
} VMEvalResult;

VMEvalResult vmEval(VM_t vm, CodeUnit *codeUnit);

void* deref(VM_t vm, Value value);

const char* getValueTypeName(VM_t vm, uint8_t type);

#endif //WARP_LANG_VM_H


