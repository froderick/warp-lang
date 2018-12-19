#ifndef WARP_LANG_VM_H
#define WARP_LANG_VM_H


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
// | :push-env   | $from   |         | ;; you can push values from the environment
// | :push-const |         | $const  | ;; you can push constant values
// | :push-heap  | $offset | $ref    | ;; you can push values from objects on the heap
// |             |         |         | ;;
// | :drop       |         |         | ;; drop the top item the stack
// | :pop-env    | $to     |         | ;; you can pop to the *local* environment
// | :pop-heap   | $offset | $ref    | ;; you can pop to objects on the heap

// | :call       |         |         | ;; you can invoke a value on the stack as a function, by first pushing
// |             |         |         | ;;   all the arguments, and then the function value reference itself
// | :call-env   | $target |         | ;; you can invoke symbols from the environment as functions
// |             |         |         | ;;
// | :ret        |         |         | ;; you can return from the currently executing function,
// |             |         |         | ;;   if there is anything on the top of the stack, it will be returned
// |             |         |         | ;;   by pushing it onto the stack of the caller
// | :jump       | $offset |         | ;; you can jump to any instuction *local* to the current function
// | :jump-if    | $offset |         | ;; you can jump to any instuction *local* to the current function, if
// |             |         |         | ;;   there is a value on the stack and it is truthy
// | :alloc      | $length | $type   | ;; you can create general objects on the heap
// | :halt       |         |         | ;; you can halt the VM, if there is anything on the top of the stack,
// |             |         |         | ;;   and it is a number, it will be returned as an exit code
// | :plus
// | :define-fn-body
// | :define-const

#include<stdint.h>
#include<wchar.h>

typedef enum InstType {
  I_PUSH_ENV,
  I_PUSH_CONST,
  I_PUSH_HEAP,
  I_DROP,
  I_POP_ENV,
  I_CALL,
  I_CALL_ENV,
  I_RET,
  I_JUMP,
  I_JUMP_IF,
  I_ALLOC,
  I_HALT,
  I_PLUS
} InstType;

typedef struct Inst {
  InstType type :  8;
  uint64_t arg1 : 56;
  uint64_t arg2 : 64;
} Inst;

typedef struct Fn {
  uint64_t nsLength;
  wchar_t *ns;

  uint64_t numLocals;
  uint64_t numArgs;

  wchar_t *name;
  uint64_t nameLength;

  uint64_t numInstructions;
  Inst *instructions;
} Fn;

#endif //WARP_LANG_VM_H

/*
 * the virtual machine at minimum needs to have a main method that accepts a file with bytecode as input
 * this file needs a format, which could just be a list of
 *
 * - function definitions, containing instructions
 * - expressions
 */









































