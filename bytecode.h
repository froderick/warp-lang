#ifndef WARP_LANG_BYTECODE_H
#define WARP_LANG_BYTECODE_H

#include <wctype.h>
#include <stdint.h>
#include <stdbool.h>
#include <wchar.h>
#include <stdio.h>

//
// Bytecode
//
// This is the basic representation of code that the virtual machine can accept as input for evaluation.

typedef struct LineNumber {
  uint64_t startInstructionIndex;
  uint64_t lineNumber;
} LineNumber;

typedef struct SourceTable {
  wchar_t *fileName;
  uint64_t numLineNumbers;
  LineNumber *lineNumbers;
} SourceTable;

typedef struct Code {
  uint16_t numLocals;           // the number of local bindings this code unit uses
  uint64_t maxOperandStackSize; // the maximum number of items this code pushes onto the operand stack at one time
  uint64_t codeLength;          // the number of bytes in this code block
  uint8_t *code;                // this code block's actual instructions
  bool hasSourceTable;
  SourceTable sourceTable;      // this lines up lines of code to generated instruction ranges
} Code;

// There are the instructions a Code object supports.

typedef enum InstType {

  I_LOAD_CONST,      // (8), typeIndex  (16) | (-> value)
  I_LOAD_LOCAL,      // (8), typeIndex  (16) | (-> value)
  I_STORE_LOCAL,     // (8), typeIndex  (16) | (objectref ->)
  I_INVOKE_DYN,      // (8)                  | (objectref, args... -> ...)
  I_INVOKE_DYN_TAIL, // (8)                  | (objectref, args... -> ...)
  I_RET,             // (8)                  | (objectref ->)
  I_CMP,             // (8)                  | (a, b -> 0 | 1)
  I_JMP,             // (8), offset (16)     | (->)
  I_JMP_IF,          // (8), offset (16)     | (value ->)
  I_JMP_IF_NOT,      // (8), offset (16)     | (value ->)
  I_ADD,             // (8)                  | (a, b -> c)
  I_SUB,             // (8)                  | (a, b -> c)
  I_DEF_VAR,         // (8), offset (16)     | (name, value ->)
  I_LOAD_VAR,        // (8), offset (16)     | (name -> value)
  I_LOAD_CLOSURE,    // (8), offset (16)     | (captures... -> value)
  I_SWAP,            // (8),                 | (a, b -> b, a)
  I_PUSH_HANDLER,    // (8), jmpAddr (16)    | (handler ->)
  I_POP_HANDLER,     // (8),                 | (->)
  I_CONS,            // (8),                 | (x, seq -> newseq)

  // requires garbage collection
      I_NEW,         // (8), objlen (16) | (-> objectref)
  I_GET_FIELD,   // (8), typeIndex  (16) | (objectref -> value)
  I_SET_FIELD,   // (8), typeIndex  (16) | (objectref, value ->)
  I_NEW_ARRAY,   // (8), objlen (16) | (arraylen -> objectref)
  I_LOAD_ARRAY,  // (8)              | (objectref, typeIndex -> value)
  I_STORE_ARRAY, // (8)              | (objectref, typeIndex, value ->)

} InstType;


// These are the constant values that can be loaded by Code instructions into the opstack for use.
// These constant values are represented as a part of the CodeUnit that is submitted to the vm for evaluation.

typedef enum ConstantType {
  CT_NONE,
  CT_BOOL,
  CT_INT,
  CT_NIL,
  CT_STR,
  CT_FN,
  CT_SYMBOL,
  CT_KEYWORD,
  CT_LIST,
  CT_VEC,
  CT_MAP
} ConstantType;

typedef struct Constant Constant;

typedef struct ConstantMetaProperty {
  uint16_t keyIndex;
  uint16_t valueIndex;
} ConstantMetaProperty;

typedef struct ConstantMeta {
  uint64_t numProperties;
  ConstantMetaProperty *properties;
} ConstantMeta;

typedef struct StringConstant {
  uint64_t length;
  wchar_t *value;
} StringConstant;

typedef struct FnConstant {
  bool hasName;
  wchar_t *name;
  uint64_t numArgs;
  bool usesVarArgs;
  uint16_t numConstants;
  Constant *constants;
  Code code;
} FnConstant;

typedef struct SymbolConstant {
  uint64_t length;
  wchar_t *value;
} SymbolConstant;

typedef struct KeywordConstant {
  uint64_t length;
  wchar_t *value;
} KeywordConstant;

typedef struct ListConstant {
  uint16_t length;
  uint16_t *constants;
  ConstantMeta meta;
} ListConstant;

typedef struct VecConstant {
  uint16_t length;
  uint16_t *constants;
} VecConstant;

typedef struct MapConstant {
  uint16_t length;
  uint16_t *constants;
  ConstantMeta meta;
} MapConstant;

typedef struct Constant {
  ConstantType type;
  union {
    uint8_t boolean;
    uint64_t integer;
    StringConstant string;
    FnConstant function;
    SymbolConstant symbol;
    KeywordConstant keyword;
    ListConstant list;
    VecConstant vec;
    MapConstant map;
  };
} Constant;

typedef struct CodeUnit {
  uint16_t numConstants;
  Constant *constants;
  Code code;
} CodeUnit;

void lineNumberInitContents(LineNumber *n);
void sourceTableInitContents(SourceTable *t);
void codeInitContents(Code *code);
void printCodeUnit(CodeUnit *unit);
void constantFnInitContents(FnConstant *fnConst);
void codeUnitInitContents(CodeUnit *codeUnit);
void constantMetaInit(ConstantMeta *c);

#endif //WARP_LANG_BYTECODE_H
