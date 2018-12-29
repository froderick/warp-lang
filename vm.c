#include <stdlib.h>
#include "vm.h"

typedef struct OpStack {
  Value *stack;
  uint64_t maxDepth;
  uint64_t usedDepth;
} OpStack;

RetVal tryOpStackInitContents(OpStack *stack, uint64_t maxDepth, Error *error) {
  RetVal ret;

  stack->maxDepth = maxDepth;
  stack->usedDepth = 0;
  tryMalloc(stack->stack, sizeof(Value) * maxDepth, "Value array");
  return R_SUCCESS;

  failure:
    return ret;
}

void opStackFreeContents(OpStack *stack) {
  if (stack != NULL) {
    stack->maxDepth = 0;
    stack->usedDepth = 0;
    if (stack->stack != NULL) {
      free(stack->stack);
      stack->stack = NULL;
    }
  }
}

RetVal tryOpStackPush(OpStack *stack, Value v, Error *error) {
  RetVal ret;

  if (stack->maxDepth == stack->usedDepth + 1) {
    throwRuntimeError(error, "cannot allocate op stack greater than max %llu", stack->maxDepth);
  }

  stack->stack[stack->usedDepth] = v;
  stack->usedDepth = stack->usedDepth + 1;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryOpStackPop(OpStack *stack, Value *ptr, Error *error) {

  RetVal ret;

  if (stack->usedDepth == 0) {
    throwRuntimeError(error, "cannot pop from empty op stack")
  }

  stack->usedDepth = stack->usedDepth - 1;
  *ptr = stack->stack[stack->usedDepth];
  return R_SUCCESS;

  failure:
    return ret;
}

typedef struct Frame {
  FunctionDefinition *functionDefinition;
  Value *locals;
  OpStack operandStack;
} Frame;

typedef struct TopLevelFrame {
  CodeUnit *codeUnit;
  Value *locals;
  OpStack opStack;
} TopLevelFrame;

typedef struct VM {

} VM;


RetVal tryVMMake(VM_t *ptr, Error *error) {

  RetVal ret;

  VM *vm;
  tryMalloc(vm, sizeof(VM), "VM");

  *ptr = vm;
  ret = R_SUCCESS;
  return ret;

  failure:
    return ret;
}

void vmFree(VM *vm) {
  if (vm != NULL) {
    free(vm);
  }
}

void topLevelFrameFreeContents(TopLevelFrame *topLevel);

RetVal tryTopLevelFrameInitContents(TopLevelFrame *topLevel, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  topLevel->codeUnit = codeUnit;
  tryMalloc(topLevel->locals, sizeof(Value) * codeUnit->code.numLocals, "Value array for locals");
  throws(tryOpStackInitContents(&topLevel->opStack, codeUnit->code.maxOperandStackSize, error));
  return R_SUCCESS;

  failure:
    topLevelFrameFreeContents(topLevel);
    return ret;
}

void topLevelFrameFreeContents(TopLevelFrame *topLevel) {
  if (topLevel != NULL) {
    if (topLevel->locals != NULL) {
      free(topLevel->locals);
      topLevel->locals = NULL;
    }
    opStackFreeContents(&topLevel->opStack);
  }
}

//typedef struct Code {
//  uint64_t numLocals;           // the number of local bindings this code unit uses
//  uint64_t maxOperandStackSize; // the maximum number of items this code pushes onto the operand stack at one time
//  uint64_t codeLength;          // the number of bytes in this code block
//  uint8_t *code;                // this code block's actual instructions
//  SourceTable sourceTable;      // this lines up lines of code to generated instruction ranges
//} Code;

//typedef struct Fn {
//  FunctionDefinition *functionDefinition;
//};

RetVal tryTopLevelFrameEval(VM *vm, TopLevelFrame *topLevel, Value *result, Error *error) {
  RetVal ret;

  Code code = topLevel->codeUnit->code;

  bool returnFound = false;
  uint16_t pc = 0;

  while (!returnFound) {
    uint8_t inst = code.code[pc];

    switch (inst) {

      case I_LOAD_CONST:  { // (8), index (16) | (-> value)

        uint16_t constantIndex = (code.code[pc + 1] << 8) | code.code[pc + 2];
        Constant constant = topLevel->codeUnit->constants[constantIndex];

        Value v;
        v.type = VT_NIL;
        v.value = 0;
        switch (constant.type) {
          case CT_BOOL:
            v.type = VT_BOOL;
            v.value = constant.boolean;
            break;
          case CT_INT:
            v.type = VT_UINT;
            v.value = constant.integer;
            break;
          case CT_NIL:
            v.type = VT_NIL;
            v.value = 0;
            break;
        }

        throws(tryOpStackPush(&topLevel->opStack, v, error));

        pc = pc + 3;
        break;
      }

      case I_LOAD_LOCAL:  { // (8), index  (16) | (-> value)
        uint16_t localIndex = *((uint16_t *)(code.code + pc + 1));
        Value v = topLevel->locals[localIndex];
        throws(tryOpStackPush(&topLevel->opStack, v, error));

        pc = pc + 3;
        break;
      }

      case I_STORE_LOCAL: { // (8), index  (16) | (objectref ->)
        uint16_t localIndex = *((uint16_t *)(code.code + pc + 1));
        Value v;
        throws(tryOpStackPop(&topLevel->opStack, &v, error));
        topLevel->locals[localIndex] = v;

        pc = pc + 3;
        break;
      }

      case I_INVOKE: {      // (8)              | (objectref, args... -> ...)
//        Value invocable;
//        throws(tryOpStackPop(&topLevel->opStack, &invocable, error));
//
//        if (invocable.type != VT_FN) {
//          throwRuntimeError(error, "cannot invoke this as a function!");
//        }

        throwRuntimeError(error, "invoke not implementec");
      }
      case I_RET: {         // (8)              | (objectref ->)
        // this is the toplevel version where the compiler pretends the toplevel code is returning from a function call
        Value v;
        throws(tryOpStackPop(&topLevel->opStack, &v, error));
        *result = v;
        returnFound = true;
        break;
      }
      case I_CMP: {         // (8)              | (a, b -> 0 | 1)
        Value a, b;
        throws(tryOpStackPop(&topLevel->opStack, &a, error));
        throws(tryOpStackPop(&topLevel->opStack, &b, error));

        Value c;
        c.type = VT_BOOL;
        c.value = false;

        if (a.type == b.type && a.value == b.value) {
          // NOTE: doesn't do equivalence on heap objects, reference identity compared only
          c.value = true;
        }

        throws(tryOpStackPush(&topLevel->opStack, c, error));

        pc = pc + 1;
        break;
      }
      case I_JMP: {         // (8), offset (16) | (->)
        uint16_t newPc = *((uint16_t *)(code.code + pc + 1));
        pc = newPc;
        break;
      }
      case I_JMP_IF: {      // (8), offset (16) | (value ->)

        Value test;
        throws(tryOpStackPop(&topLevel->opStack, &test, error));

        bool truthy;
        if (test.type == VT_BOOL) {
          truthy = test.value > 0;
        }
        else if (test.type == VT_UINT) {
          truthy = test.value > 0;
        }
        else if (test.type == VT_NIL) {
          truthy = false;
        }
        else {
          throwRuntimeError(error, "unhandled truth");
        }

        if (truthy) {
          uint16_t newPc = *((uint16_t *) (code.code + pc + 1));
          pc = newPc;
        }

        break;
      }
      case I_PLUS: {        // (8)              | (a, b -> c)
        Value a, b;
        throws(tryOpStackPop(&topLevel->opStack, &a, error));
        throws(tryOpStackPop(&topLevel->opStack, &b, error));

        if (a.type != VT_UINT || b.type != VT_UINT) {
          throwRuntimeError(error, "can only add two integers");
        }

        Value c;
        c.type = VT_UINT;
        c.value = a.value + b.value;

        throws(tryOpStackPush(&topLevel->opStack, c, error));

        pc = pc + 1;
        break;
      }
      case I_HALT: {        // (8)              | (exitcode ->)
        throwRuntimeError(error, "halt unimplemented");
      }
      case I_DEF_VAR: {     // (8)              | (name, value ->)
        throwRuntimeError(error, "def var unimplemented");
      }
      case I_LOAD_VAR: {    // (8)              | (name -> value)
        throwRuntimeError(error, "load var unimplemented");
      }

      // requires garbage collection
      case I_NEW:           // (8), objlen (16) | (-> objectref)
      case I_GET_FIELD:     // (8), index  (16) | (objectref -> value)
      case I_SET_FIELD:     // (8), index  (16) | (objectref, value ->)
      case I_NEW_ARRAY:     // (8), objlen (16) | (arraylen -> objectref)
      case I_LOAD_ARRAY:    // (8)              | (objectref, index -> value)
      case I_STORE_ARRAY:   // (8)              | (objectref, index, value ->)
        throwRuntimeError(error, "instruction unimplemented");
        break;
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, Value *result, Error *error) {

  RetVal ret;

  TopLevelFrame *topLevel;
  tryMalloc(topLevel, sizeof(TopLevelFrame), "TopLevelFrame")
  throws(tryTopLevelFrameInitContents(topLevel, codeUnit, error));

  throws(tryTopLevelFrameEval(vm, topLevel, result, error));

  topLevelFrameFreeContents(topLevel);
  ret = R_SUCCESS;
  return ret;

  failure:
    if (topLevel != NULL) {
      topLevelFrameFreeContents(topLevel);
    }
    return ret;
}





































//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Value Spec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

// In this machine, all values are represented by a 64-bit word.
//
// The leftmost 3 bits are used to encode the following types. The remaining 61
// bits are interpreted on a per-type basis.
//
// :unsigned-int - an overflowable unsigned integer
// :bool         - 0 for false, 1 for true
// :nil          - constant, always 0
// :char         - the lowest 32 bits represent a UTF-16 character
// :object       - interpreted as an unsigned integer, the value is a pointer
//                 offset to dynamically-allocated memory on the heap.
//
// Objects on the heap are represented this way:
//
// [56 bits - total object size in words][8 bits - specific type of object] [...]
//
// Here are the object types:
//
// :char-array (0)   - The first word is an unsigned integer containing the
//                     number of characters in the string. Each subsequent word
//                     contains up to two UTF-16 characters, one in the higher 32
//                     bits and one in the lower 32 bits. This is an optimization
//                     for representing Strings.
//
// :object-array (1) - The first word is an unsigned integer containing the
//                     number of characters in the string. Each subsequent word
//                     contains a value.
//
// :record-type (2)  - Describes the names of the fields in a record, and their
//                     indexes.
//
// :record (3)       - The first word is the Value that describes the record-type
//                     for a record. The rest of the words are values that
//                     describe the record's fields.
//
// :function (5)     - The first word is the number of arguments the function
//                     accepts. The second word is a string value that is the
//                     source code for the function. The remainder of the words
//                     are instructions, which are represented as word singles
//                     or triples: main instruction, arg hint, arg
//

// TODO: how do we represent lists in the virtual machine? are they implemented on-top of arrays/records? (YES)
// TODO: is a function reference represented differently from a lambda?
// TODO: there appears to be no meaningful difference between args and locals within the vm, they are all just locals
// though captured variables are different, since they may get boxed as part of a lambda


/*
 * When the vm is asked to evaluate code, it always evaluates it in the context of a specific namespace. The symbols
 * defined in this namespace form the basis of the evaluation environment. Also part of the environment is
 * pre-allocated space for the locals the code will introduce. Last, the code will require an operand stack to do
 * anything useful, so one is pre-allocated based on the stack usage the code requires.
 *
 * When the vm evaluates code, it expects that the top-level expression will terminate in a `ret` instruction so it
 * will know the result of the expression it evaluated. The compiler must detect that a form is a top-level
 * form and generate this extra instruction as needed.
 */

//F_CONST,   -> I_LOAD_*
//F_IF,      -> I_JMP_IF
//F_LET,     -> I_STORE_LOCAL
//F_DEF,     -> I_DEFINE
//F_ENV_REF, -> I_LOAD_LOCAL
//F_VAR_REF, -> I_LOAD_VAR
//F_FN,     -> defined fns
//F_BUILTIN,
//F_FN_CALL -> I_INVOKE

/*
 * tryVMEval(vm, code, &result); // the result is a Value, which the caller can then introspect
 *
 * load code into VM as temporary zero-argument function within current namespace
 * invoke function
 * destroy function
 */

/*
 * the virtual machine at minimum needs to have a main method that accepts a file with bytecode as input
 * this file needs a format, which could just be a sequence of sexprs where each one represents either:
 *
 * - a function definition, containing instructions
 * - an expression, which is just a bag of instructions with no
 */

/* notes transcription:
 *
 * What does a VM need to be useful?
 *
 * - It needs to expose access to the runtime from executing bytecode:
 *
 */

/*
 * So I've been thinking about how to proceed with this VM business...
 *
 * I'm thinking of:
 * - doing the full virtual machine
 * - except just using the current ast as an input
 * - and also doing the vm as a repl, where basically you call an API in a single-threaded fashion to evaluate code
 * - there will be a true, command-line repl, but there will also be an API-based repl for the compiler to use
 * - if using the AST verbatim as the instruction format becomes hard, then I'll know how the bytecode should be different
 *   from the AST.
 * - other than the input format, everything else about the internals of the vm should be as if bytecode was fed in
 *   instead of AST forms. this includes the registers, the operations on the registers, and the runtime.
 *
 * *PERHAPS*: I should just do the fucking instruction set and be done with it
 */
