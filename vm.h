#ifndef WARP_LANG_VM_H
#define WARP_LANG_VM_H

#include<stdint.h>
#include<wchar.h>
#include <stdbool.h>

#include "source.h"
#include "errors.h"
#include "reader.h"

// This is the basic representation of code that the virtual machine can accept as input for evaluation.

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
  I_INVOKE_DYN,      // (8)              | (objectref, args... -> ...)
  I_INVOKE_DYN_TAIL, // (8)              | (objectref, args... -> ...)
  I_RET,             // (8)              | (objectref ->)
  I_CMP,             // (8)              | (a, b -> 0 | 1)
  I_JMP,             // (8), offset (16) | (->)
  I_JMP_IF,          // (8), offset (16) | (value ->)
  I_JMP_IF_NOT,      // (8), offset (16) | (value ->)
  I_ADD,             // (8)              | (a, b -> c)
  I_SUB,             // (8)              | (a, b -> c)
  I_DEF_VAR,         // (8), offset (16) | (name, value ->)
  I_LOAD_VAR,        // (8), offset (16) | (name -> value)
  I_LOAD_CLOSURE,    // (8), offset (16) | (captures... -> value)
  I_SWAP,            // (8),             | (a, b -> b, a)
  I_SET_HANDLER,     // (8),             | (jumpAddr, handler ->)
  I_CLEAR_HANDLER,   // (8),             | (->)

  I_CONS,            // (8),             | (x, seq -> newseq)
  I_FIRST,           // (8),             | (seq -> x)
  I_REST,            // (8),             | (seq -> seq)
  I_SET_MACRO,       // (8),             | (name -> nil)
  I_GET_MACRO,       // (8),             | (name -> bool)

  // requires garbage collection
  I_NEW,         // (8), objlen (16) | (-> objectref)
  I_GET_FIELD,   // (8), typeIndex  (16) | (objectref -> value)
  I_SET_FIELD,   // (8), typeIndex  (16) | (objectref, value ->)
  I_NEW_ARRAY,   // (8), objlen (16) | (arraylen -> objectref)
  I_LOAD_ARRAY,  // (8)              | (objectref, typeIndex -> value)
  I_STORE_ARRAY, // (8)              | (objectref, typeIndex, value ->)

} InstType;

void printCodeArray(uint8_t *code, uint16_t codeLength);

// These are the constant values that can be loaded by Code instructions into the opstack for use.
// These constant values are represented as a part of the CodeUnit that is submitted to the vm for evaluation.

typedef enum ConstantType {
  CT_NONE,
  CT_BOOL,
  CT_INT,
  CT_NIL,
  CT_STR,
  CT_FN,
  CT_VAR_REF,
  CT_SYMBOL,
  CT_KEYWORD,
  CT_LIST,
  CT_FN_REF,
} ConstantType;

typedef struct Constant Constant;

typedef struct StringConstant {
  uint64_t length;
  wchar_t *value;
} StringConstant;

typedef struct FnConstant {
  uint64_t fnId;
  bool hasName;
  Text name;
  uint64_t numArgs;
  bool usesVarArgs;
  uint16_t numConstants;
  uint16_t numCaptures;
  Constant *constants;
  Code code;
} FnConstant;

typedef struct VarRefConstant {
  uint64_t nameLength;
  wchar_t *name;
} VarRefConstant;

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
} ListConstant;

typedef struct FnRefConstant {
  uint64_t fnId;
} FnRefConstant;

typedef struct Constant {
  ConstantType type;
  union {
    uint8_t boolean;
    uint64_t integer;
    StringConstant string;
    FnConstant function;
    VarRefConstant varRef;
    SymbolConstant symbol;
    KeywordConstant keyword;
    ListConstant list;
    FnRefConstant fnRef;
  };
} Constant;

typedef struct CodeUnit {
  uint16_t numConstants;
  Constant *constants;
  Code code;
} CodeUnit;


void sourceTableInitContents(SourceTable *t);

void codeInitContents(Code *code);
void codeFreeContents(Code *code);
void printCodeUnit(CodeUnit *unit);

void constantFnInitContents(FnConstant *fnConst);
void _constantFreeContents(Constant *c);

void codeUnitInitContents(CodeUnit *codeUnit);
void codeUnitFreeContents(CodeUnit *codeUnit);

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
  VT_NIL,
  VT_UINT,
  VT_BOOL,
  VT_FN,
//  VT_CHAR,
  VT_STR,
  VT_SYMBOL,
  VT_KEYWORD,
  VT_LIST,
  VT_CLOSURE,
//  VT_OBJECT,
} ValueType;

typedef struct Value {
  ValueType type : 4;
  uint64_t value : 60;
} Value;

RetVal tryVMInitContents(VM_t vm, Error *error);
void vmFreeContents(VM_t vm);
RetVal tryVMMake(VM_t *ptr , Error *error);
void vmFree(VM_t vm);

typedef struct VMExceptionFrame {
  Text fileName;
  uint64_t lineNumber;
  Text functionName;
} VMExceptionFrame;

typedef struct VMExceptionFrames {
  uint64_t length;
  VMExceptionFrame *elements;
} VMExceptionFrames;

typedef struct VMException {
  Text message;
  VMExceptionFrames frames;
} VMException;

typedef enum VMEvalResultType {
  RT_NONE,
  RT_RESULT,
  RT_EXCEPTION
} VMEvalResultType;

typedef struct VMEvalResult {
  VMEvalResultType type;
  union {
    Expr result;
    VMException exception;
  };
} VMEvalResult;

void exceptionInitContents(VMException *e);
void exceptionFreeContents(VMException *e);
void evalResultInitContents(VMEvalResult *r);
void evalResultFreeContents(VMEvalResult *r);

RetVal tryExceptionPrint(VMException *e, wchar_t **ptr, Error *error);
RetVal tryExceptionPrintf(VMException *e, Error *error);

RetVal tryVMEval(VM_t vm, CodeUnit *codeUnit, VMEvalResult *result, Error *error);

#endif //WARP_LANG_VM_H











































