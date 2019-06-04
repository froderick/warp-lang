## VM Rework

This document describes a re-design of the warp instruction set and natively supported 
values and data structures.

### Values

#### Immediate

Immediate values are values where the 3 least-significant bits are non-zero.

NIL
UINT (add, sub)
BOOL
CHAR // note: chars are utf16, but loaded and written as utf8

#### Non-Immediate

Non-immediate values are values where the 3 least-significant bits are zero. They are direct pointers to the values
they reference in the heap. This is achieved by 8-byte padding allocations of heap objects.

// strings are not mutable, arrays are mutable, but they share the same basic structure. it is the semantics that are different
// byte-arrays and char-arrays do not require traversal for gc
// general arrays are traversed for gc

PAIR (car, cdr, pair)
FN (invoke_dyn, invoke_dyn_tail, load_fn)
CLOSURE (invoke_dyn, invoke_dyn_tail, load_closure)
BYTEARRAY(byte_array, length, load_array, store_array)
CHARARRAY(char_array, length, load_array, store_array)
FXARRAY (fxarray, length, load_array, store_array)
ARRAY (array, length, load_array, store_array)
STRING (bless_string, length, load_array, store_array) // note: strings are utf16, but loaded and written as utf8
SYMBOL (symbol, symbol_name) // note: symbols are interned
KEYWORD (keyword, keyword_name) // note: keywords are interned
CFN (invoke_dyn, invoke_dyn_tail)
RECORD (record, record_type, load_record, store_record)
PORT (port_open_file, port_close, port_read, port_write)

### Instructions

LOAD_CONST,      // (8), typeIndex  (16) | (-> value)
LOAD_LOCAL,      // (8), typeIndex  (16) | (-> value)
STORE_LOCAL,     // (8), typeIndex  (16) | (objectref ->)
RET,             // (8)                  | (objectref ->)
CMP,             // (8)                  | (a, b -> 0 | 1)
JMP,             // (8), offset (16)     | (->)
JMP_IF,          // (8), offset (16)     | (value ->)
JMP_IF_NOT,      // (8), offset (16)     | (value ->)
DEF_VAR,         // (8), offset (16)     | (name, value ->)
LOAD_VAR,        // (8), offset (16)     | (name -> value)
SWAP,            // (8),                 | (a, b -> b, a)

ADD,             // (8)                  | (uint, uint -> uint)
SUB,             // (8)                  | (uint, uint -> uint)

PAIR,            // (8),                 | (car, cdr -> pair)
CAR,             // (8),                 | (pair -> car)
CDR,             // (8),                 | (pair -> cdr)

LOAD_STRING,     // (8),                 | (bytevector -> string)
LENGTH,          // (8),                 | (vector or string -> uint)

SYMBOL,          // (8),                 | (string -> symbol)
SYMBOL_NAME,     // (8),                 | (symbol -> string)

INVOKE_DYN,      // (8)                  | (objectref, args... -> ...)
INVOKE_DYN_TAIL, // (8)                  | (objectref, args... -> ...)
LOAD_FN          // (8),                 | (bytevector -> fn)
LOAD_CLOSURE,    // (8), offset (16)     | (captures... -> value)

NEW,             // (8), objlen (16)     | (-> objectref)
GET_FIELD,       // (8), typeIndex (16)  | (objectref -> value)
SET_FIELD,       // (8), typeIndex (16)  | (objectref, value ->)
NEW_ARRAY,       // (8), objlen (16)     | (arraylen -> objectref)
LOAD_ARRAY,      // (8)                  | (objectref, typeIndex -> value)
STORE_ARRAY,     // (8)                  | (objectref, typeIndex, value ->)

### Missing Language Functionality

- need a general (efficient) way of looping (loop/recur or named let)
- need language-level vector support
