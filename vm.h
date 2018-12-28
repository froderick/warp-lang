#ifndef WARP_LANG_VM_H
#define WARP_LANG_VM_H

#include<stdint.h>
#include<wchar.h>
#include <stdbool.h>

#include "source.h"
#include "errors.h"

typedef enum InstType {

  I_LOAD_CONST,  // (8), index  (16) | (-> value)

  I_LOAD_LOCAL,  // (8), index  (16) | (-> value)
  I_STORE_LOCAL, // (8), index  (16) | (objectref ->)
  I_INVOKE,      // (8)              | (objectref, args... -> ...)
  I_RET,         // (8)              | (objectref ->)
  I_CMP,         // (8)              | (a, b -> 0 | 1)
  I_JMP,         // (8), offset (16) | (->)
  I_JMP_IF,      // (8), offset (16) | (value ->)
  I_HALT,        // (8)              | (exitcode ->)
  I_PLUS,        // (8)              | (a, b -> c)
  I_DEF_VAR,     // (8)              | (name, value ->)
  I_LOAD_VAR,    // (8)              | (name -> value)

  // requires garbage collection
  I_NEW,         // (8), objlen (16) | (-> objectref)
  I_GET_FIELD,   // (8), index  (16) | (objectref -> value)
  I_SET_FIELD,   // (8), index  (16) | (objectref, value ->)
  I_NEW_ARRAY,   // (8), objlen (16) | (arraylen -> objectref)
  I_LOAD_ARRAY,  // (8)              | (objectref, index -> value)
  I_STORE_ARRAY, // (8)              | (objectref, index, value ->)

} InstType;

typedef enum ConstantType {
  CT_BOOL,
  CT_INT,
  CT_NIL
} ConstantType;

typedef struct Constant {
  ConstantType type;
  union {
    uint8_t boolean;
    uint64_t integer;
    // TODO: strings
  };
} Constant;

typedef struct LineNumber {
  uint64_t startInstructionIndex;
  uint64_t lineNumber;
} LineNumber;

typedef struct SourceTable {
  uint64_t fileNameLength;
  wchar_t *fileName;
  uint64_t numLineNumbers;
  LineNumber *lineNumbers;
} SourceTable;

typedef struct Code {
  uint64_t numLocals;           // the number of local bindings this code unit uses
  uint64_t maxOperandStackSize; // the maximum number of items this code pushes onto the operand stack at one time
  uint64_t codeLength;          // the number of bytes in this code block
  uint8_t *code;                // this code block's actual instructions
  SourceTable sourceTable;      // this lines up lines of code to generated instruction ranges
} Code;

typedef struct FunctionDefinition { // this is the runtime definition of a function
  uint64_t nsLength;
  wchar_t *ns;
  uint64_t nameLength;
  wchar_t *name;
  uint64_t numArgs;
  Code code;
} FunctionDefinition;

typedef struct CodeUnit {
  uint64_t numConstants;
  Constant *constants;
  uint64_t numFunctionDefinitions;
  FunctionDefinition *functionDefinitions;
  Code code;
} CodeUnit;

typedef struct VM *VM_t;

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
  VT_NONE,
  VT_UINT,
  VT_BOOL,
  VT_NIL,
  VT_CHAR,
  VT_OBJECT
} ValueType;

typedef struct Value {
  ValueType type : 4;
  uint64_t value : 60
} Value;

RetVal tryVMEval(VM_t vm, CodeUnit *codeUnit, Value *result, Error *error);

#endif //WARP_LANG_VM_H











































