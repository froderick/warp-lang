#ifndef WARP_LANG_VM_H
#define WARP_LANG_VM_H

#include "source.h"

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
// Here is the function definition:
//
// [ns name num-args num-locals instructions]
//
// Here is the instruction architecture:
//
// |   8 bits    | 56 bits | 64 bits |
// +------------ +---------+---------+
// | :push-fn-arg      | $from   |         | ;; you can push function arguments to the param stack
// | :push-fn-local    | $from   |         | ;; you can push function locals to the param stack
// | :push-fn-captured | $from   |         | ;; you can push function captured variables to the param stack
// | :push-const       |         | $const  | ;; you can push constant values to the param stack
// | :push-heap        | $offset | $ref    | ;; you can push values from specific fields in objects on the heap to the param stack
// |                   |         |         | ;;
// | :pop-all          |         |         | ;; drop all the items from the param stack
// |                   |         |         | ;;
// | :move             |         |         | ;; drop the top item the param stack
// | :pop-fn-local     | $to     |         | ;; you can pop to function locals from the param stack
// | :pop-heap         | $offset | $ref    | ;; you can pop to specific fields within objects on the heap from the param stack

// | :call             |         |         | ;; you can invoke a value on the stack as a function, by first pushing
// |                   |         |         | ;;   all the arguments, and then the function value reference itself
// | :call-env         | $target |         | ;; you can invoke symbols from the environment as functions
// |                   |         |         | ;;
// | :ret              |         |         | ;; you can return from the currently executing function,
// |                   |         |         | ;;   if there is anything on the top of the stack, it will be returned
// |                   |         |         | ;;   by pushing it onto the stack of the caller
// | :jump             | $offset |         | ;; you can jump to any instuction *local* to the current function
// | :jump-if          | $offset |         | ;; you can jump to any instuction *local* to the current function, if
// |                   |         |         | ;;   there is a value on the stack and it is truthy
// | :alloc            | $length | $type   | ;; you can create general objects on the heap
// | :halt             |         |         | ;; you can halt the VM, if there is anything on the top of the stack,
// |                   |         |         | ;;   and it is a number, it will be returned as an exit code
// | :plus
// | :define-fn-body
// | :define-const

/*
 * what registers do we have? all are 64 bits
 *
 * ax - the accumulator register. results of function calls go here
 * stack - function arguments are pushed into this, it is cleared when a function returns
 * ip - the instruction pointer
 * r1, r2, r3? just r1?
 */

/*
 * What kinds of memory can we operate on?
 *
 * - function arguments
 * - function locals
 * - function captured variables
 * - the function-local or global param stack
 * - vars
 * - gc'd heap objects
 * - constant values
 * - the accumulator register
 */

// TODO: how do we represent lists in the virtual machine? are they implemented on-top of arrays/records?

// TODO: is a function reference represented differently from a lambda?

// TODO: there appears to be no meaningful difference between args and locals within the vm, they are all just locals
// though captured variables are different, since they may get boxed as part of a lambda

/*
 * What kinds of instructions do we support?
 *
 * load-local  (8), index  (16) | (-> value)
 * store-local (8), index  (16) | (objectref ->)
 * get-field   (8), index  (16) | (objectref -> value)
 * set-field   (8), index  (16) | (objectref, value ->)
 * load-const  (8), val    (64) | (-> value)
 * load-array  (8)              | (objectref, index -> value)
 * store-array (8)              | (objectref, index, value ->)
 * invoke      (8)              | (objectref, args... -> ...)
 * ret         (8)              | (objectref ->)
 * new         (8), objlen (16) | (-> objectref)
 * newarray    (8), objlen (16) | (arraylen -> objectref)
 * cmp         (8)              | (a, b -> 0 | 1)
 * jmp         (8), offset (16) | (->)
 * jmp-if      (8), offset (16) | (value ->)
 * plus        (8)              | (a, b -> c)
 */

#include<stdint.h>
#include<wchar.h>
#include <stdbool.h>

#include "errors.h"

typedef enum InstType {

  I_LOAD_UINT,   // (8), val    (64) | (-> value)
  I_LOAD_BOOL,   // (8), val    (8)  | (-> value)
  I_LOAD_NIL,    // (8)         (8)  | (-> value)

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

typedef struct Code {
  uint64_t codeLength;          // the number of bytes in this code block
  uint8_t *code;                // this code block's actual instructions
} Code;

typedef struct Fn { // this is the runtime definition of a function
  uint64_t nsLength;
  wchar_t *ns;
  uint64_t nameLength;
  wchar_t *name;
  uint64_t numArgs;
  Code code;
} Fn;

typedef struct CodeUnit {

  // TODO: constants
  // TODO: source info

  uint64_t numFns;
  Fn *fns;

  uint64_t numLocals;           // the number of local bindings this code unit uses
  uint64_t maxOperandStackSize; // the maximum number of items this code pushes onto the operand stack at one time

} CodeUnit;

typedef struct Value {
  uint8_t type : 3;
  uint64_t value : 61
} Value;

typedef struct VM *VM_t;

RetVal tryVMEval(VM_t vm, CodeUnit *codeUnit, Value *result);


//F_CONST,
//F_IF,
//F_LET,
//F_DEF,
//F_ENV_REF,
//F_VAR_REF,
//F_FN,     ->
//F_BUILTIN,
//F_FN_CALL -> I_INVOKE

#endif //WARP_LANG_VM_H

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










































