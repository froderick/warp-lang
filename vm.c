  #include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include "vm.h"
#include "utils.h"

/*
 * CodeUnit init/free functions
 */

void lineNumberInitContents(LineNumber *n) {
  n->lineNumber = 0;
  n->startInstructionIndex = 0;
}

void sourceTableInitContents(SourceTable *t) {
  t->lineNumbers = NULL;
  t->numLineNumbers = 0;
  textInitContents(&t->fileName);
}

void sourceTableFreeContents(SourceTable *t) {
  textFreeContents(&t->fileName);
  t->numLineNumbers = 0;
  if (t->lineNumbers != NULL) {
    free(t->lineNumbers);
    t->lineNumbers = NULL;
  }
}

void codeInitContents(Code *code) {
  code->maxOperandStackSize = 0;
  code->numLocals = 0;
  code->hasSourceTable = false;
  code->code = NULL;
  code->codeLength = 0;
  sourceTableInitContents(&code->sourceTable);
}

void codeFreeContents(Code *code) {
  if (code != NULL) {

    code->numLocals = 0;
    code->maxOperandStackSize = 0;
    code->codeLength = 0;

    if (code->code != NULL) {
      free(code->code);
      code->code = NULL;
    }

    if (code->hasSourceTable) {
      code->hasSourceTable = false;
      sourceTableFreeContents(&code->sourceTable);
    }
  }
}

void constantFnInitContents(FnConstant *fnConst) {
  fnConst->fnId = 0;
  fnConst->hasName = 0;
  textInitContents(&fnConst->name);
  fnConst->numArgs = 0;
  fnConst->usesVarArgs = false;
  fnConst->numConstants = 0;
  fnConst->numCaptures = 0;
  fnConst->constants = NULL;
  codeInitContents(&fnConst->code);
}

void constantFnFreeContents(FnConstant *fnConst) {
  if (fnConst != NULL) {
    fnConst->numArgs = 0;
    if (fnConst->constants != NULL) {
      for (int i = 0; i < fnConst->numConstants; i++) {
        _constantFreeContents(&fnConst->constants[i]);
      }
      fnConst->numConstants = 0;
      free(fnConst->constants);
      fnConst->constants = NULL;
    }
    codeFreeContents(&fnConst->code);
  }
}

void _constantFreeContents(Constant *c) {
  if (c != NULL) {
    switch (c->type) {
      case CT_NONE:
      case CT_NIL:
        break;
      case CT_BOOL:
        c->boolean = false;
        break;
      case CT_INT:
        c->integer = 0;
        break;
      case CT_VAR_REF:
        c->varRef.nameLength = 0;
        if (c->varRef.name != NULL) {
          free(c->varRef.name);
          c->varRef.name = NULL;
        }
        break;
      case CT_STR:
        c->string.length = 0;
        if (c->string.value != NULL) {
          free(c->string.value);
          c->string.value = NULL;
        }
        break;
      case CT_FN:
        constantFnFreeContents(&c->function);
        break;
      case CT_SYMBOL:
        c->symbol.length = 0;
        if (c->symbol.value != NULL) {
          free(c->symbol.value);
          c->symbol.value = NULL;
        }
        break;
      case CT_KEYWORD:
        c->keyword.length = 0;
        if (c->keyword.value != NULL) {
          free(c->keyword.value);
          c->keyword.value = NULL;
        }
        break;
      case CT_LIST:
        c->list.length = 0;
        if (c->list.constants != NULL) {
          free(c->list.constants);
          c->list.constants = NULL;
        }
        break;
      case CT_FN_REF:
        break;
    }
  }
}

void codeUnitInitContents(CodeUnit *codeUnit) {
  codeUnit->constants = NULL;
  codeUnit->numConstants = 0;
  codeInitContents(&codeUnit->code);
}

void codeUnitFreeContents(CodeUnit *u) {
  if (u != NULL) {
    if (u->constants != NULL) {
      for (int i = 0; i < u->numConstants; i++) {
        _constantFreeContents(&u->constants[i]);
      }
      u->numConstants = 0;
      free(u->constants);
      u->constants = NULL;
    }
    codeFreeContents(&u->code);
  }
}

RetVal tryCodeDeepCopy(Code *from, Code *to, Error *error) {
  RetVal ret;

  to->numLocals = from->numLocals;
  to->maxOperandStackSize = from->maxOperandStackSize;
  to->codeLength = from->codeLength;

  tryMalloc(to->code, sizeof(uint8_t) * to->codeLength, "Code array");
  memcpy(to->code, from->code, to->codeLength);

  to->hasSourceTable = from->hasSourceTable;
  if (to->hasSourceTable) {
    throws(tryTextCopy(&from->sourceTable.fileName, &to->sourceTable.fileName, error));

    to->sourceTable.numLineNumbers = from->sourceTable.numLineNumbers;
    uint64_t numbersSize = sizeof(LineNumber) * to->sourceTable.numLineNumbers;
    tryMalloc(to->sourceTable.lineNumbers, numbersSize, "LineNumber array");
    memcpy(to->sourceTable.lineNumbers, from->sourceTable.lineNumbers, numbersSize);
  }

  return R_SUCCESS;

  failure:
  codeFreeContents(to);
  return ret;
}

void exFrameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

void exFrameFreeContents(VMExceptionFrame *f) {
  if (f != NULL) {
    textFreeContents(&f->functionName);
    f->lineNumber = 0;
    textFreeContents(&f->fileName);
  }
}

void framesInitContents(VMExceptionFrames *f) {
  f->length = 0;
  f->elements = NULL;
}

void framesFreeContents(VMExceptionFrames *f) {
  if (f != NULL) {
    if (f->elements != NULL) {
      for (uint64_t i = 0; i < f->length; i++) {
        VMExceptionFrame *fr = &f->elements[i];
        exFrameFreeContents(fr);
      }
      free(f->elements);
      f->length = 0;
      f->elements = NULL;
    }
  }
}

void _frameInitContents(VMExceptionFrame *f) {
  textInitContents(&f->functionName);
  f->unknownSource = true;
  f->lineNumber = 0;
  textInitContents(&f->fileName);
}

void exceptionInitContents(VMException *e) {
  textInitContents(&e->message);
  framesInitContents(&e->frames);
}

void exceptionFreeContents(VMException *e) {
  if (e != NULL) {
    textFreeContents(&e->message);
    framesFreeContents(&e->frames);
  }
}

void evalResultInitContents(VMEvalResult *r) {
  r->type = RT_NONE;
}

void evalResultFreeContents(VMEvalResult *r) {
  if (r != NULL) {
    switch (r->type){
      case RT_RESULT:
        exprFreeContents(&r->result);
        break;
      case RT_EXCEPTION:
        exceptionFreeContents(&r->exception);
        break;
      case RT_NONE:
        break;
    }
  }
}

/*
 * VM Data Structures
 */

// gc

/*
 * The constant part of a function is the FnDef. This is what gets hydrated at eval time.
 * The variable part of a function is the Fn. It holds a value reference to the FnDef?
 */

typedef struct Fn {
  bool hasName;
  uint64_t nameLength;
  wchar_t *name;
  uint16_t numCaptures;
  uint16_t numArgs;
  bool usesVarArgs;

  uint16_t numConstants;
  Value *constants;

  uint16_t numLocals;           // the number of local bindings this code unit uses
  uint64_t maxOperandStackSize; // the maximum number of items this code pushes onto the operand stack at one time
  uint64_t codeLength;          // the number of bytes in this code block
  uint8_t *code;                // this code block's actual instructions

  bool hasSourceTable;
  uint64_t sourceFileNameLength;
  wchar_t *sourceFileName;
  uint64_t numLineNumbers;
  LineNumber *lineNumbers;

} Fn;

typedef struct Closure {
  Value fn;
  uint16_t numCaptures;
  Value *captures;
} Closure;

typedef struct String {
  uint64_t length;
  wchar_t *value;
} String;

typedef struct Symbol {
  uint64_t length;
  wchar_t *value;
} Symbol;

typedef struct Keyword {
  uint64_t length;
  wchar_t *value;
} Keyword;

typedef struct Cons {
  Value value;
  Value next; // this must be a Cons, or Nil
} Cons;

typedef struct GC {

  uint64_t allocatedFnSpace;
  uint64_t usedFnSpace;
  Fn *fns;

  uint64_t allocatedClosureSpace;
  uint64_t usedClosureSpace;
  Closure *closures;

  uint64_t allocatedStringSpace;
  uint64_t usedStringSpace;
  String *strings;

  uint64_t allocatedSymbolSpace;
  uint64_t usedSymbolSpace;
  Symbol *symbols;

  uint64_t allocatedKeywordSpace;
  uint64_t usedKeywordSpace;
  Keyword *keywords;

  uint64_t allocatedConsSpace;
  uint64_t usedConsSpace;
  Cons *conses;

} GC;

typedef struct GC1 {

  // the total memory allocated
  uint64_t heapMemorySize;
  void *heapMemory;

  // the actual heaps
  uint64_t heapSize;
  void *heapA; // the first half of the memory
  void *heapB; // the second half of the memory

  // the current heap
  void *currentHeap; // the heap to use for allocation
  void *allocPtr;    // the offset within the heap to use for allocation

} GC1;

// namespaces

typedef struct Var {
  wchar_t *namespace;
  wchar_t *name;
  Value value;
  bool isMacro;
} Var;

typedef struct VarList {
  uint64_t allocatedLength;
  uint64_t length;
  Var *vars;
} VarList;

typedef struct Namespace {
  wchar_t *name;
  VarList localVars;
  Var *importedVars;
  uint64_t numImportedVars;
} Namespace;

typedef struct Namespaces {
  Namespace *namespaces;
  uint64_t numNamespaces;
  Namespace *currentNamespace;
} Namespaces;

// instruction definitions

typedef struct ExecFrame *ExecFrame_t;
typedef RetVal (*TryEval) (struct VM *vm, ExecFrame_t frame, Error *error);

typedef struct Inst {
  const char *name;
  void (*print)(int *i, const char* name, uint8_t *code);
  TryEval tryEval;
} Inst;

typedef struct InstTable {
  uint8_t numInstructions;
  Inst instructions[256];
} InstTable;

// vm state

typedef struct VM {
  GC gc;
  GC1 gc1;
  Namespaces namespaces;
  InstTable instTable;
} VM;

// frames

typedef struct OpStack {
  Value *stack;
  uint64_t maxDepth;
  uint64_t usedDepth;
} OpStack;

typedef struct TopLevelFrame {
  uint16_t numConstants;
  Value *constants;
  Code code;
  uint16_t numLocals;
  Value *locals;
  OpStack opStack;
} TopLevelFrame;

/*
 * Common value factories
 */

Value nil() {
  Value v;
  v.type = VT_NIL;
  v.value = 0;
  return v;
}

/*
 * NEW alloc/gc impl
 *
 * super useful: http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm
 */

void GC1FreeContents(GC1 *gc) {
  free(gc->heapMemory);
  gc->heapMemory = NULL;
  gc->heapA = NULL;
  gc->heapB = NULL;
  gc->currentHeap = NULL;
  gc->allocPtr = NULL;
}

void GC1InitContents(GC1 *gc) {
  gc->heapMemorySize = 0;
  gc->heapMemory = NULL;
  gc->heapSize = 0;
  gc->heapA = NULL;
  gc->heapB = NULL;
  gc->currentHeap = NULL;
  gc->allocPtr = NULL;
}

RetVal tryGC1InitContents(GC1 *gc, uint64_t maxHeapSize, Error *error) {
  RetVal ret;

  GC1InitContents(gc);

  gc->heapSize = maxHeapSize;
  gc->heapMemorySize = gc->heapSize * 2;

  tryMalloc(gc->heapMemory, gc->heapMemorySize, "GC memory");
  memset(gc->heapMemory, 0, gc->heapMemorySize);

  gc->heapA = gc->heapMemory;
  gc->heapB = gc->heapA + gc->heapSize;
  gc->currentHeap = gc->heapA;
  gc->allocPtr = gc->currentHeap;

  return R_SUCCESS;
  failure:
    return ret;
}

/*
 * if heap has space for allocated value, allocate:
 *   write object at allocPtr
 *   save allocPtr as new value location
 *   increase allocPtr by length of object
 *
 * else
 *   collect
 *   if heap has space for allocated value, allocate
 *   else fail
 */

RetVal alloc(GC1 *gc, uint64_t length, void **ptr, uint64_t *offset, Error *error) {
  RetVal ret;

  uint64_t heapUsed = gc->allocPtr - gc->currentHeap;
  if (heapUsed + length < gc->heapSize) {
    *ptr = gc->allocPtr;
    gc->allocPtr += length;
  }
  else {
    throwRuntimeError(error, "collect() not supported yet");
  }

  *offset = *ptr - gc->currentHeap;

  return R_SUCCESS;
  failure:
  return ret;
}
//RetVal alloc(GC1 *gc, uint64_t length, void **ptr, uint64_t *offset, Error *error) {
//  RetVal ret;
//
//  uint64_t heapAvailable = gc->allocPtr - gc->currentHeap;
//  if (heapAvailable + length < gc->heapSize) {
//    *ptr = gc->allocPtr;
//    gc->allocPtr += length;
//  }
//  else {
//    throwRuntimeError(error, "collect() not supported yet");
//  }
//
//  *offset = *ptr - gc->currentHeap;
//
//  return R_SUCCESS;
//  failure:
//    return ret;
//}

RetVal deref(GC1 *gc, void **ptr, uint64_t offset, Error *error) {
  RetVal ret;

  if (offset > gc->heapSize) {
    throwRuntimeError(error, "invalid memory address");
  }

  *ptr = gc->currentHeap + offset;

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal collect(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  void *scanptr;
  void *allocptr;


  return R_SUCCESS;
  failure:
  return ret;
}

/*
 * gc/runtime implementation
 */

void GCInit(GC *gc) {

  gc->usedClosureSpace = 0;
  gc->allocatedClosureSpace = 0;
  gc->closures = NULL;

  gc->allocatedStringSpace = 0;
  gc->usedStringSpace = 0;
  gc->strings = NULL;

  gc->allocatedSymbolSpace = 0;
  gc->usedSymbolSpace = 0;
  gc->symbols = NULL;

  gc->allocatedKeywordSpace = 0;
  gc->usedKeywordSpace = 0;
  gc->keywords = NULL;

  gc->allocatedConsSpace = 0;
  gc->usedConsSpace = 0;
  gc->conses = NULL;
}

void _stringFreeContents(String *str);
void _symbolFreeContents(Symbol *s);
void _keywordFreeContents(Keyword *k);
void _consFreeContents(Cons *c);

void GCFreeContents(GC *gc) {
  if (gc != NULL) {

    free(gc->closures);
    gc->closures = NULL;
    gc->usedClosureSpace = 0;
    gc->allocatedClosureSpace = 0;

    for (uint64_t i=0; i<gc->usedStringSpace; i++) {
      _stringFreeContents(&gc->strings[i]);
    }
    free(gc->strings);
    gc->strings = NULL;
    gc->usedStringSpace = 0;
    gc->allocatedStringSpace = 0;

    for (uint64_t i=0; i<gc->usedSymbolSpace; i++) {
      _symbolFreeContents(&gc->symbols[i]);
    }
    free(gc->symbols);
    gc->symbols = NULL;
    gc->usedSymbolSpace = 0;
    gc->allocatedSymbolSpace = 0;

    for (uint64_t i=0; i<gc->usedKeywordSpace; i++) {
      _keywordFreeContents(&gc->keywords[i]);
    }
    free(gc->keywords);
    gc->keywords = NULL;
    gc->usedKeywordSpace = 0;
    gc->allocatedKeywordSpace = 0;

    for (uint64_t i=0; i<gc->usedConsSpace; i++) {
      _consFreeContents(&gc->conses[i]);
    }
    free(gc->conses);
    gc->conses = NULL;
    gc->usedConsSpace = 0;
    gc->allocatedConsSpace = 0;
  }
}

RetVal tryAllocateClosure(GC *gc, Closure closure, Value *value, Error *error) {
  RetVal ret;

  if (gc->closures == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->closures, len * sizeof(Closure), "Closure array");
    gc->allocatedClosureSpace = len;
  }
  else if (gc->usedClosureSpace == gc->allocatedClosureSpace) {
    uint64_t newAllocatedLength = gc->allocatedClosureSpace * 2;

    Closure* resizedClosures = realloc(gc->closures, newAllocatedLength * sizeof(Closure));
    if (resizedClosures == NULL) {
      ret = memoryError(error, "realloc Closure array");
      goto failure;
    }

    gc->allocatedClosureSpace = newAllocatedLength;
    gc->closures = resizedClosures;
  }

  uint64_t index = gc->usedClosureSpace;
  gc->closures[index] = closure;
  gc->usedClosureSpace = index + 1;

  value->type = VT_CLOSURE;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

void _closureFreeContents(Closure *closure) {
  if (closure != NULL) {
    if (closure->captures != NULL) {
      free(closure->captures);
      closure->captures = NULL;
    }
  }
}

RetVal tryDerefClosure(GC *gc, Value value, Closure *closure, Error *error) {
  RetVal ret;

  if (gc->usedClosureSpace <= value.value) {
    throwInternalError(error, "closure reference points to closure that does not exist");
  }

  *closure = gc->closures[value.value];
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryAllocateString(GC *gc, String str, Value *value, Error *error) {
  RetVal ret;

  if (gc->strings == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->strings, len * sizeof(String), "String array");
    gc->allocatedStringSpace = len;
  }
  else if (gc->usedStringSpace == gc->allocatedStringSpace) {
    uint64_t newAllocatedLength = gc->allocatedStringSpace * 2;

    String* resizedStrings = realloc(gc->strings, newAllocatedLength * sizeof(String));
    if (resizedStrings == NULL) {
      ret = memoryError(error, "realloc String array");
      goto failure;
    }

    gc->allocatedStringSpace = newAllocatedLength;
    gc->strings = resizedStrings;
  }

  uint64_t index = gc->usedStringSpace;
  gc->strings[index] = str;
  gc->usedStringSpace = index + 1;

  value->type = VT_STR;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryDerefString(GC *gc, Value value, String *str, Error *error) {
  RetVal ret;

  if (gc->usedStringSpace <= value.value) {
    throwInternalError(error, "str reference points to str that does not exist");
  }

  *str = gc->strings[value.value];
  return R_SUCCESS;

  failure:
  return ret;
}

void _stringFreeContents(String *str) {
  if (str != NULL) {
    if (str->value != NULL) {
      free(str->value);
      str->value = NULL;
    }
    str->length = 0;
  }
}

RetVal tryAllocateSymbol(GC *gc, Symbol sym, Value *value, Error *error) {
  RetVal ret;

  if (gc->symbols == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->symbols, len * sizeof(Symbol), "Symbol array");
    gc->allocatedSymbolSpace = len;
  }
  else if (gc->usedSymbolSpace == gc->allocatedSymbolSpace) {
    uint64_t newAllocatedLength = gc->allocatedSymbolSpace * 2;

    Symbol* resizedSymbols = realloc(gc->symbols, newAllocatedLength * sizeof(Symbol));
    if (resizedSymbols == NULL) {
      ret = memoryError(error, "realloc Symbol array");
      goto failure;
    }

    gc->allocatedSymbolSpace = newAllocatedLength;
    gc->symbols = resizedSymbols;
  }

  uint64_t index = gc->usedSymbolSpace;
  gc->symbols[index] = sym;
  gc->usedSymbolSpace = index + 1;

  value->type = VT_SYMBOL;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryDerefSymbol(GC *gc, Value value, Symbol *sym, Error *error) {
  RetVal ret;

  if (gc->usedSymbolSpace <= value.value) {
    throwInternalError(error, "sym reference points to sym that does not exist");
  }

  *sym = gc->symbols[value.value];
  return R_SUCCESS;

  failure:
  return ret;
}

void _symbolFreeContents(Symbol *s) {
  if (s != NULL) {
    if (s ->value != NULL) {
      free(s->value);
      s->value = NULL;
    }
    s->length = 0;
  }
}

RetVal tryAllocateKeyword(GC *gc, Keyword kw, Value *value, Error *error) {
  RetVal ret;

  if (gc->keywords == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->keywords, len * sizeof(Keyword), "Keyword array");
    gc->allocatedKeywordSpace = len;
  }
  else if (gc->usedKeywordSpace == gc->allocatedKeywordSpace) {
    uint64_t newAllocatedLength = gc->allocatedKeywordSpace * 2;

    Keyword* resizedKeywords = realloc(gc->keywords, newAllocatedLength * sizeof(Keyword));
    if (resizedKeywords == NULL) {
      ret = memoryError(error, "realloc Keyword array");
      goto failure;
    }

    gc->allocatedKeywordSpace = newAllocatedLength;
    gc->keywords = resizedKeywords;
  }

  uint64_t index = gc->usedKeywordSpace;
  gc->keywords[index] = kw;
  gc->usedKeywordSpace = index + 1;

  value->type = VT_KEYWORD;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryDerefKeyword(GC *gc, Value value, Keyword *kw, Error *error) {
  RetVal ret;

  if (gc->usedKeywordSpace <= value.value) {
    throwInternalError(error, "kw reference points to kw that does not exist");
  }

  *kw = gc->keywords[value.value];
  return R_SUCCESS;

  failure:
  return ret;
}

void _keywordFreeContents(Keyword *k) {
  if (k != NULL) {
    if (k ->value != NULL) {
      free(k->value);
      k->value = NULL;
    }
    k->length = 0;
  }
}

RetVal tryAllocateCons(GC *gc, Cons cons, Value *value, Error *error) {
  RetVal ret;

  if (gc->conses == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->conses, len * sizeof(Cons), "Cons array");
    gc->allocatedConsSpace = len;
  }
  else if (gc->usedConsSpace == gc->allocatedConsSpace) {
    uint64_t newAllocatedLength = gc->allocatedConsSpace * 2;

    Cons* resizedConses = realloc(gc->conses, newAllocatedLength * sizeof(Cons));
    if (resizedConses == NULL) {
      ret = memoryError(error, "realloc Cons array");
      goto failure;
    }

    gc->allocatedConsSpace = newAllocatedLength;
    gc->conses = resizedConses;
  }

  uint64_t index = gc->usedConsSpace;
  gc->conses[index] = cons;
  gc->usedConsSpace = index + 1;

  value->type = VT_LIST;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryDerefCons(GC *gc, Value value, Cons *cons, Error *error) {
  RetVal ret;

  if (gc->usedConsSpace <= value.value) {
    throwInternalError(error, "cons reference points to cons that does not exist");
  }

  *cons = gc->conses[value.value];
  return R_SUCCESS;

  failure:
  return ret;
}

void _consFreeContents(Cons *c) {
  // nothing to do
}

/*
 * Loading Constants as Values
 */

typedef struct UnresolvedFnRef {
  FnRefConstant fnRef;
  uint16_t constantIndex;
} UnresolvedFnRef;

typedef struct UnresolvedFnRefs {
  uint64_t allocatedSpace;
  uint64_t usedSpace;
  UnresolvedFnRef *references;
} UnresolvedFnRefs;

void unresolvedFnRefsInitContents(UnresolvedFnRefs *r) {
  r->allocatedSpace = 0;
  r->usedSpace = 0;
  r->references = NULL;
}

void unresolvedFnRefsFreeContents(UnresolvedFnRefs *r) {
  if (r != NULL) {
    r->allocatedSpace = 0;
    r->usedSpace = 0;
    if (r->references != NULL) {
      free(r->references);
      r->references = NULL;
    }
  }
}

RetVal tryUnresolvedFnRefsAppend(UnresolvedFnRefs *references, UnresolvedFnRef ref, Error *error) {
  RetVal ret;

  if (references->references == NULL) {
    uint16_t len = 16;
    tryMalloc(references->references, len * sizeof(UnresolvedFnRef), "UnresolvedFnRef array");
    references->allocatedSpace = len;
  }
  else if (references->usedSpace == references->allocatedSpace) {
    uint64_t newAllocatedLength = references->allocatedSpace * 2;

    UnresolvedFnRef* resizedReferences = realloc(references->references, newAllocatedLength * sizeof(UnresolvedFnRef));
    if (resizedReferences == NULL) {
      ret = memoryError(error, "realloc UnresolvedFnRef array");
      goto failure;
    }

    references->allocatedSpace = newAllocatedLength;
    references->references = resizedReferences;
  }

  uint64_t index = references->usedSpace;
  references->references[index] = ref;
  references->usedSpace = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryHydrateConstant(VM *vm, Value *alreadyHydratedConstants, Constant c, Value *ptr, uint16_t constantIndex, UnresolvedFnRefs *unresolved, Error *error);

RetVal _tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value *values,
                            UnresolvedFnRefs *unresolved, Error *error);

void fnInitContents(Fn *fn) {

  fn->hasName = false;
  fn->nameLength = 0;
  fn->name = NULL;
  fn->numCaptures = 0;
  fn->numArgs = 0;
  fn->usesVarArgs = false;
  fn->numConstants = 0;
  fn->constants = NULL;

  fn->numLocals = 0;
  fn->maxOperandStackSize = 0;
  fn->codeLength = 0;
  fn->code = NULL;

  fn->hasSourceTable = false;
  fn->sourceFileNameLength = 0;
  fn->sourceFileName = NULL;
  fn->numLineNumbers = 0;
  fn->lineNumbers = NULL;
}

RetVal tryFnHydrate(VM *vm, FnConstant *fnConst, Value *value, Error *error) {
  RetVal ret;

  // always clean up
  UnresolvedFnRefs unresolved;
  unresolvedFnRefsInitContents(&unresolved);

  // cleanup on failure
  Fn *fn = NULL;

  size_t nameSize = (fnConst->name.length + 1) * sizeof(wchar_t);
  size_t constantsSize = fnConst->numConstants * sizeof(Value);
  size_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
  size_t sourceFileNameSize = (fnConst->code.sourceTable.fileName.length + 1) * sizeof(wchar_t);
  size_t lineNumbersSize = fnConst->code.sourceTable.numLineNumbers * sizeof(LineNumber);

  size_t fnSize = sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize + lineNumbersSize;

  uint64_t offset = 0;
  throws(alloc(&vm->gc1, fnSize, (void*)&fn, &offset, error));

  value->type = VT_FN;
  value->value = offset;

  fnInitContents(fn);

  void *base = fn;
  fn->name           = base + sizeof(Fn);
  fn->constants      = base + sizeof(Fn) + nameSize;
  fn->code           = base + sizeof(Fn) + nameSize + constantsSize;
  fn->sourceFileName = base + sizeof(Fn) + nameSize + constantsSize + codeSize;
  fn->lineNumbers    = base + sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize;

  fn->hasName = fnConst->hasName;
  if (fn->hasName) {
    fn->nameLength = fnConst->name.length;
    memcpy(fn->name, fnConst->name.value, nameSize);
    fn->name[fn->nameLength] = L'\0';
  }

  fn->numCaptures = fnConst->numCaptures;
  fn->numArgs = fnConst->numArgs;
  fn->usesVarArgs = fnConst->usesVarArgs;

  {
    fn->numConstants = fnConst->numConstants;
    throws(_tryHydrateConstants(vm, fn->numConstants, fnConst->constants, fn->constants, &unresolved, error));

    for (uint16_t i=0; i<unresolved.usedSpace; i++) {
      UnresolvedFnRef ref = unresolved.references[i];

      if (ref.fnRef.fnId == fnConst->fnId) {
        // resolve the reference
        fn->constants[ref.constantIndex] = *value;
      }
      else {
        throwRuntimeError(error, "cannot hydrate a reference to a function other than the current one: %llu", ref.fnRef.fnId);
      }
    }
  }

  fn->numLocals = fnConst->code.numLocals;
  fn->maxOperandStackSize = fnConst->code.maxOperandStackSize;

  {
    fn->codeLength = fnConst->code.codeLength;
    memcpy(fn->code, fnConst->code.code, codeSize);
  }

  fn->hasSourceTable = fnConst->code.hasSourceTable;
  if (fn->hasSourceTable) {

      fn->sourceFileNameLength = fnConst->code.sourceTable.fileName.length;
      memcpy(fn->sourceFileName, fnConst->code.sourceTable.fileName.value, sourceFileNameSize);
      fn->sourceFileName[fn->sourceFileNameLength] = L'\0';

      fn->numLineNumbers = fnConst->code.sourceTable.numLineNumbers;
      memcpy(fn->lineNumbers, fnConst->code.sourceTable.lineNumbers, lineNumbersSize);
  }

  unresolvedFnRefsFreeContents(&unresolved);

  return R_SUCCESS;

  failure:
    unresolvedFnRefsFreeContents(&unresolved);
    return ret;
}

RetVal tryStringHydrate(VM *vm, StringConstant strConst, Value *value, Error *error) {
  RetVal ret;

  String str;
  str.length = strConst.length;
  throws(tryCopyText(strConst.value, &str.value, str.length, error));
  throws(tryAllocateString(&vm->gc, str, value, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVarRefHydrate(VM *vm, VarRefConstant varRefConst, Value *value, Error *error) {
  RetVal ret;

  String str;
  str.length = varRefConst.nameLength;
  throws(tryCopyText(varRefConst.name, &str.value, str.length, error));
  throws(tryAllocateString(&vm->gc, str, value, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal trySymbolHydrate(VM *vm, SymbolConstant symConst, Value *value, Error *error) {
  RetVal ret;

  Symbol sym;
  sym.length = symConst.length;
  throws(tryCopyText(symConst.value, &sym.value, sym.length, error));
  throws(tryAllocateSymbol(&vm->gc, sym, value, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryKeywordHydrate(VM *vm, KeywordConstant kwConst, Value *value, Error *error) {
  RetVal ret;

  Keyword kw;
  kw.length = kwConst.length;
  throws(tryCopyText(kwConst.value, &kw.value, kw.length, error));
  throws(tryAllocateKeyword(&vm->gc, kw, value, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryListHydrate(VM *vm, Value *alreadyHydratedConstants, ListConstant listConst, Value *value, Error *error) {
  RetVal ret;

  // build up list with conses

  Value seq = nil();

  for (uint16_t i=0; i < listConst.length; i++) {

    uint16_t listConstEnd = listConst.length - 1;
    uint16_t valueIndex = listConst.constants[listConstEnd - i];

    Cons cons;
    cons.value = alreadyHydratedConstants[valueIndex];
    cons.next = seq;

    throws(tryAllocateCons(&vm->gc, cons, &seq, error));
  }

  *value = seq;
  return R_SUCCESS;

  failure:
  return ret;
}

// TODO: when hydrating a function, determine its fn declaration depth and keep these in a mapping table after hydration is complete
// TODO: while hydrating, keep a growing list of the constant specs and pointers to the values that should reference functions
// TODO: after hydrating, iterate over the unresolved references, match them up by typeIndex

// during analysis
// - grant a unique id to each function
// - within the function, when adding a binding for the function name, use this id for the binding typeIndex
// during compilation
// - include function ids in functions
// - include function ids in constants that reference functions
// during hydration
// - collect a list of function references, along with pointers to their intended value locations, while hydrating a function
// - before completing hydration, resolve any matching references with the function's hydrated value
// - explode if there are any references that the current function can't satisfy, since we don't support closures yet

RetVal tryHydrateConstant(VM *vm, Value *alreadyHydratedConstants, Constant c, Value *ptr, uint16_t constantIndex, UnresolvedFnRefs *unresolved, Error *error) {
  RetVal ret;

  Value v;

  switch (c.type) {
    case CT_BOOL:
      v.type = VT_BOOL;
      v.value = c.boolean;
      break;
    case CT_INT:
      v.type = VT_UINT;
      v.value = c.integer;
      break;
    case CT_NIL:
      v.type = VT_NIL;
      v.value = 0;
      break;
    case CT_FN:
      throws(tryFnHydrate(vm, &c.function, &v, error));
      break;
    case CT_VAR_REF:
      throws(tryVarRefHydrate(vm, c.varRef, &v, error));
      break;
    case CT_STR:
      throws(tryStringHydrate(vm, c.string, &v, error));
      break;
    case CT_SYMBOL:
      throws(trySymbolHydrate(vm, c.symbol, &v, error));
      break;
    case CT_KEYWORD:
      throws(tryKeywordHydrate(vm, c.keyword, &v, error));
      break;
    case CT_LIST:
      throws(tryListHydrate(vm, alreadyHydratedConstants, c.list, &v, error));
      break;
    case CT_FN_REF: {

      // capture this for later
      UnresolvedFnRef ref;
      ref.fnRef = c.fnRef;
      ref.constantIndex = constantIndex;
      throws(tryUnresolvedFnRefsAppend(unresolved, ref, error));

      // leave the value nil for now
      v.type = VT_NIL;
      v.value = 0;

      break;
    }
    case CT_NONE:
    default:
      throwInternalError(error, "invalid constant: %u", c.type);
  }

  *ptr = v;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal _tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value *values,
                            UnresolvedFnRefs *unresolved, Error *error) {
  RetVal ret;

  for (uint16_t i=0; i<numConstants; i++) {

    Constant c = constants[i];
    Value v;

    throws(tryHydrateConstant(vm, values, c, &v, i, unresolved, error));

    values[i] = v;
  }

  return R_SUCCESS;
  failure:
    return ret;
}

RetVal tryHydrateConstants(VM *vm, Value *constants, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  UnresolvedFnRefs unresolved;
  unresolvedFnRefsInitContents(&unresolved);

  throws(_tryHydrateConstants(vm, codeUnit->numConstants, codeUnit->constants, constants, &unresolved, error));

  if (unresolved.usedSpace > 0) {
    throwRuntimeError(error, "no unresolved function references should be present in the top level");
  }

  ret = R_SUCCESS;
  goto done;

  failure:
  goto done;

  done:
    unresolvedFnRefsFreeContents(&unresolved);
    return ret;
}

/*
 * Create a reader representation of a Value (an Expr).
 *
 * Some representations are approximate and cannot be round-tripped through eval, such as functions and closures.
 */
RetVal tryVMPrn(VM *vm, Value result, Expr *expr, Error *error) {
  RetVal ret;

  switch (result.type) {
    case VT_NIL:
      expr->type = N_NIL;
      break;
    case VT_UINT: {
      expr->type = N_NUMBER;
      expr->number.value = result.value;
      break;
    }
    case VT_BOOL:
      expr->type = N_BOOLEAN;
      expr->boolean.value = result.value;
      break;
    case VT_FN: {
      expr->type = N_STRING;
      wchar_t function[] = L"<function>";
      expr->string.length = wcslen(function);
      throws(tryCopyText(function, &expr->string.value, expr->string.length, error));
      break;
    }
    case VT_CLOSURE: {
      expr->type = N_STRING;
      wchar_t function[] = L"<closure>";
      expr->string.length = wcslen(function);
      throws(tryCopyText(function, &expr->string.value, expr->string.length, error));
      break;
    }
    case VT_STR: {
      String str;
      throws(tryDerefString(&vm->gc, result, &str, error));

      expr->type = N_STRING;
      expr->string.length = str.length;
      throws(tryCopyText(str.value, &expr->string.value, expr->string.length, error));
      break;
    }
    case VT_SYMBOL: {
      Symbol sym;
      throws(tryDerefSymbol(&vm->gc, result, &sym, error));

      expr->type = N_SYMBOL;
      expr->symbol.length = sym.length;
      throws(tryCopyText(sym.value, &expr->symbol.value, expr->string.length, error));
      break;
    }
    case VT_KEYWORD: {
      Keyword kw;
      throws(tryDerefKeyword(&vm->gc, result, &kw, error));

      expr->type = N_KEYWORD;
      expr->keyword.length = kw.length;
      throws(tryCopyText(kw.value, &expr->keyword.value, expr->string.length, error));
      break;
    }
    case VT_LIST: {
      Cons cons;
      throws(tryDerefCons(&vm->gc, result, &cons, error));

      expr->type = N_LIST;
      listInitContents(&expr->list);
      Expr *elem;

      tryMalloc(elem, sizeof(Expr), "Expr");
      throws(tryVMPrn(vm, cons.value, elem, error));
      throws(tryListAppend(&expr->list, elem, error));

      while (cons.next.type != VT_NIL) {
        throws(tryDerefCons(&vm->gc, cons.next, &cons, error));
        tryMalloc(elem, sizeof(Expr), "Expr");
        throws(tryVMPrn(vm, cons.value, elem, error));
        throws(tryListAppend(&expr->list, elem, error));
      }

      break;
    }
    default:
    throwRuntimeError(error, "unsuported value type: %u", result.type);
  }

  return R_SUCCESS;

  failure:
  exprFree(expr);
  return ret;
}

/*
 * Managing namespaces of vars
 */

void varFree(Var *var);

void varInitContents(Var *var) {
  var->namespace = NULL;
  var->name = NULL;
  var->value.type = VT_NIL;
  var->value.value = 0;
  var->isMacro = false;
}

RetVal tryVarInit(wchar_t *namespace, wchar_t *name, Value value, Var *var, Error *error) {
  RetVal ret;

  varInitContents(var);

  throws(tryCopyText(namespace, &var->namespace, wcslen(namespace), error));
  throws(tryCopyText(name, &var->name, wcslen(name), error));
  var->value = value;

  return R_SUCCESS;

  failure:
  varFree(var);
  return ret;
}

void varFreeContents(Var *var) {
  if (var != NULL) {
    if (var->namespace != NULL) {
      free(var->namespace);
      var->namespace = NULL;
    }
    if (var->name != NULL) {
      free(var->name);
      var->name = NULL;
    }
  }
}

void varFree(Var *var) {
  if (var != NULL) {
    varFreeContents(var);
    free(var);
  }
}

bool resolveVar(Namespaces *analyzer, wchar_t *symbolName, uint64_t symbolNameLength, Var **var) {

  Namespace *ns;
  wchar_t *searchName;
  uint64_t searchNameLength;

  // assume unqualified namespace at first
  ns = analyzer->currentNamespace;
  searchName = symbolName;
  searchNameLength = symbolNameLength;

  if (symbolNameLength > 2) { // need at least three characters to qualify a var name with a namespace: ('q/n')

    // attempt to find a qualified namespace
    wchar_t *slashPtr = wcschr(symbolName, L'/');
    if (slashPtr != NULL) { // qualified
      uint64_t nsLen = slashPtr - symbolName;

      for (int i=0; i<analyzer->numNamespaces; i++) {

        Namespace *thisNs = &analyzer->namespaces[i];
        if (wcsncmp(symbolName, thisNs->name, nsLen) == 0) {
          ns = thisNs;
          searchName = slashPtr + 1;
          searchNameLength = symbolNameLength - (nsLen + 1);
          break;
        }
      }
    }
  }

  // find var within namespace

  for (int i=0; i<ns->localVars.length; i++) {
    if (wcscmp(searchName, ns->localVars.vars[i].name) == 0) {
      *var = &ns->localVars.vars[i];
      return true;
    }
  }

  return false;
}

void varListInit(VarList *list) {
  list->length = 0;
  list->allocatedLength = 0;
  list->vars = NULL;
}

void varListFreeContents(VarList *list) {
  if (list != NULL) {
    if (list->vars != NULL) {
      for (int i=0; i<list->length; i++) {
        varFreeContents(&list->vars[i]);
      }
      free(list->vars);
      list->vars = NULL;
      list->length = 0;
      list->allocatedLength = 0;
    }
  }
}

RetVal tryAppendVar(VarList *list, Var var, Error* error) {

  RetVal ret;

  if (list->vars == NULL) {
    uint64_t len = 16;
    tryMalloc(list->vars, len * sizeof(Var), "Var array");
    list->allocatedLength = len;
  }
  else if (list->length == list->allocatedLength) {
    uint64_t newAllocatedLength = list->allocatedLength * 2;

    Var* resizedVars = realloc(list->vars, newAllocatedLength * sizeof(Var));
    if (resizedVars == NULL) {
      ret = memoryError(error, "realloc Var array");
      goto failure;
    }

    list->allocatedLength = newAllocatedLength;
    list->vars = resizedVars;
  }

  list->vars[list->length] = var;
  list->length = list->length + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryDefVar(Namespaces *namespaces, wchar_t *symbolName, uint64_t symbolNameLength, Value value, Error *error) {
  RetVal ret;

  // gets cleaned up on failure
  Var createdVar;
  varInitContents(&createdVar);

  Var *resolvedVar = NULL;
  if (!resolveVar(namespaces, symbolName, symbolNameLength, &resolvedVar)) {
    Namespace *ns = namespaces->currentNamespace;
    throws(tryVarInit(ns->name, symbolName, value, &createdVar, error));
    throws(tryAppendVar(&ns->localVars, createdVar, error));
  }
  else {
    resolvedVar->value = value;
  }

  return R_SUCCESS;

  failure:
  varFreeContents(&createdVar);
  return ret;
}

RetVal tryNamespaceMake(wchar_t *name, uint64_t length, Namespace **ptr , Error *error) {

  RetVal ret;

  Namespace *ns;
  tryMalloc(ns, sizeof(Namespace), "Namespace");

  ns->name = NULL;
  ns->importedVars = NULL;
  ns->numImportedVars = 0;

  varListInit(&ns->localVars);

  throws(tryCopyText(name, &ns->name, length, error));

  *ptr = ns;
  return R_SUCCESS;

  failure:
  if (ns != NULL) {
    free(ns);
  }
  return ret;
}

void namespaceFreeContents(Namespace *ns) {
  if (ns != NULL) {
    free(ns->name);
    free(ns->importedVars);
    varListFreeContents(&ns->localVars);
  }
}

RetVal tryNamespacesInitContents(Namespaces *namespaces, Error *error) {

  RetVal ret;

  Namespace *userNs;
  throws(tryNamespaceMake(L"user", wcslen(L"user"), &userNs, error));

  namespaces->currentNamespace = userNs;
  namespaces->namespaces = userNs;
  namespaces->numNamespaces = 1;

  return R_SUCCESS;

  failure:
  return ret;
}

void namespacesFreeContents(Namespaces *namespaces) {
  if (namespaces != NULL) {
    namespaces->currentNamespace = NULL;
    if (namespaces->namespaces != NULL) {
      for (int i=0; i<namespaces->numNamespaces; i++) {
        namespaceFreeContents(&namespaces->namespaces[i]);
      }
      free(namespaces->namespaces);
    }
  }
}

/*
 * The ExecFrame and operations it supports
 */

RetVal readInstruction(ExecFrame_t frame, uint8_t *ptr, Error *error);
RetVal readIndex(ExecFrame_t frame, uint16_t *ptr, Error *error);
RetVal setPc(ExecFrame_t frame, uint16_t newPc, Error *error);
RetVal getConst(ExecFrame_t frame, uint16_t constantIndex, Value *ptr, Error *error);
RetVal getLocal(ExecFrame_t frame, uint16_t localIndex, Value *ptr, Error *error);
RetVal setLocal(ExecFrame_t frame, uint16_t localIndex, Value value, Error *error);
uint16_t pushOperand(ExecFrame_t frame, Value value, Error *error);
uint16_t popOperand(ExecFrame_t frame, Value *value, Error *error);

bool hasResult(ExecFrame_t frame);
bool hasParent(ExecFrame_t frame);
RetVal getParent(ExecFrame_t frame, ExecFrame_t *ptr, Error *error);
RetVal setResult(ExecFrame_t frame, Value result, Error *error);
RetVal getResult(ExecFrame_t frame, Value *ptr, Error *error);

typedef struct ExceptionHandler {
  uint16_t jumpAddress;
  uint16_t localIndex;
} ExceptionHandler;

bool hasHandler(ExecFrame_t frame);
RetVal getHandler(ExecFrame_t frame, ExceptionHandler *ptr, Error *error);
void setHandler(ExecFrame_t frame, ExceptionHandler handler);
void clearHandler(ExecFrame_t frame);

bool hasFnName(ExecFrame_t frame);
RetVal getFnName(ExecFrame_t frame, Text *name, Error *error);

bool hasSourceTable(ExecFrame_t frame);
bool getLineNumber(ExecFrame_t frame, uint64_t *lineNumber);
bool getFileName(ExecFrame_t frame, Text *fileName);

bool hasException(ExecFrame_t frame);
void setException(ExecFrame_t frame, VMException e);
RetVal getException(ExecFrame_t frame, VMException *e, Error *error);

RetVal pushFrame(VM *vm, ExecFrame_t *frame, Value newFn, Error *error);
RetVal replaceFrame(VM *vm, ExecFrame_t frame, Value newFn, Error *error);
void popFrame(ExecFrame_t frame);

/*
 * Instruction Definitions
 */

// (8), typeIndex (16) | (-> value)
RetVal tryLoadConstEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex;
  Value constant;

  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &constant, error));
  throws(pushOperand(frame, constant, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), typeIndex (16) | (-> value)
RetVal tryLoadLocalEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t localIndex;
  Value v;

  throws(readIndex(frame, &localIndex, error));
  throws(getLocal(frame, localIndex, &v, error));
  throws(pushOperand(frame, v, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), typeIndex  (16) | (objectref ->)
RetVal tryStoreLocalEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t localIndex;
  Value v;

  throws(readIndex(frame, &localIndex, error));
  throws(popOperand(frame, &v, error));
  throws(setLocal(frame, localIndex, v, error));

  return R_SUCCESS;

  failure:
  return ret;
}

/*
 * The invocable should become the contract needed to init a frame on the stack
 *
 * TODO: let's make a separate heap (perm-gen) for compiled, loaded code
 * - this would mean that we can depend on the locations of functions not changing due to garbage collection
 *   while regular instructions within a function are being executed.
 * - we would garbage collect functions whenever the space is exhausted, which would be caused by attempting to
 *   load new code. we'd start from the roots like normal gc, just with different semantics
 * - the only time functions would get moved is when loading more code, which triggers gc.
 * - this lets us write vm code that keeps direct pointers to hydrated compiled code
 *
 * - see https://www.quora.com/What-are-some-best-practices-in-using-the-Java-8-JVM-Metaspace
 */

typedef struct Invocable {
  Value fnRef;
  Fn *fn;
  bool hasClosure;
  Closure closure;
} Invocable;

RetVal tryPopInvocable(VM *vm, ExecFrame_t frame, Invocable *invocable, Error *error) {
  RetVal ret;

  throws(popOperand(frame, &invocable->fnRef, error));
  Value pop = nil();
  throws(popOperand(frame, &pop, error));

  invocable->fnRef = pop;

  switch (invocable->fnRef.type) {
    case VT_FN: {

      throws(deref(&vm->gc1, (void*)&invocable->fn, invocable->fnRef.value, error));

      invocable->hasClosure = false;
      invocable->closure.fn = nil();
      invocable->closure.numCaptures = 0;
      invocable->closure.captures = NULL;
      break;
    }
    case VT_CLOSURE: {
      throws(tryDerefClosure(&vm->gc, invocable->fnRef, &invocable->closure, error));
      invocable->hasClosure = true;
      throws(deref(&vm->gc1, (void*)&invocable->fn, invocable->closure.fn.value, error));
      break;
    }
    default:
      // fail: not all values are invocable
      throwRuntimeError(error, "cannot invoke this value type as a function: %u", invocable->fnRef.type);
  }

  return R_SUCCESS;

  failure:
  return ret;
}

//* - vm honors var-arg flag
//*   - sees var-arg flag on invocable
//*   - pops all static arguments into local slot
//*   - pops number indicating number of extra arguments
//*   - we *aways* pass the number of arguments
//*   - pops number of extra arguments into a list, sets as final argument in local slot

RetVal tryInvokePopulateLocals(VM *vm, ExecFrame_t parent, ExecFrame_t child, Invocable invocable, Error *error) {
  RetVal ret;

  Value numArgsSupplied;
  throws(popOperand(parent, &numArgsSupplied, error));

  if (numArgsSupplied.type != VT_UINT) {
    throwRuntimeError(error, "first argument must be number of arguments supplied: %u", numArgsSupplied.type);
  }

  if (numArgsSupplied.value > invocable.fn->numArgs) {

    if (!invocable.fn->usesVarArgs) {
      // fail: wrong number of arguments
      throwRuntimeError(error, "extra arguments supplied, expected %u but got %llu", invocable.fn->numArgs,
          numArgsSupplied.value);
    }

    // read the extra args into a list, push it back on the stack

    Value seq = nil();
    uint16_t numVarArgs = (numArgsSupplied.value - invocable.fn->numArgs) + 1;
    for (uint16_t i = 0; i < numVarArgs; i++) {

      Value arg;
      throws(popOperand(parent, &arg, error));

      Cons cons;
      cons.value = arg;
      cons.next = seq;
      throws(tryAllocateCons(&vm->gc, cons, &seq, error));
    }

    throws(pushOperand(parent, seq, error));
  }

  if (numArgsSupplied.value == invocable.fn->numArgs && invocable.fn->usesVarArgs) {
    // wrap the last arg in a list

    Value arg;
    throws(popOperand(parent, &arg, error));

    Value seq = nil();

    Cons cons;
    cons.value = arg;
    cons.next = nil();
    throws(tryAllocateCons(&vm->gc, cons, &seq, error));

    throws(pushOperand(parent, seq, error));
  }

  if (numArgsSupplied.value < invocable.fn->numArgs) {

    if (!invocable.fn->usesVarArgs) {
      // fail: wrong number of arguments
      throwRuntimeError(error, "required arguments not supplied, expected %u but got %llu", invocable.fn->numArgs,
                        numArgsSupplied.value);
    }

    // make sure the list is present on the stack

    throws(pushOperand(parent, nil(), error));
  }

  for (uint16_t i = 0; i < invocable.fn->numArgs; i++) {
    Value arg;
    throws(popOperand(parent, &arg, error));

    uint16_t idx = invocable.fn->numArgs - (1 + i);
    throws(setLocal(child, idx, arg, error));
  }

  if (invocable.fn->numCaptures > 0) {

    if (!invocable.hasClosure) {
      throwRuntimeError(error, "cannot invoke this fn without a closure, it captures variables: %u", invocable.fn->numCaptures);
    }
    if (invocable.closure.numCaptures < invocable.fn->numCaptures) {
      throwRuntimeError(error, "closure does not have enough captured variables: %u", invocable.closure.numCaptures);
    }

    uint16_t nextLocalIdx = invocable.fn->numArgs;
    for (uint16_t i=0; i<invocable.fn->numCaptures; i++) {
      throws(setLocal(child, nextLocalIdx, invocable.closure.captures[i], error));
      nextLocalIdx = nextLocalIdx + 1;
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (objectref, args... -> ...)
RetVal tryInvokeDynEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  // for cleanup on failure
  bool pushed = false;

  Invocable invocable;
  throws(tryPopInvocable(vm, frame, &invocable, error));

  throws(pushFrame(vm, &frame, invocable.fnRef, error));
  pushed = true;

  ExecFrame_t parent;
  throws(getParent(frame, &parent, error));

  throws(tryInvokePopulateLocals(vm, parent, frame, invocable, error));

  return R_SUCCESS;

  failure:
    if (pushed) {
      popFrame(frame);
    }
    return ret;
}

/*
 * tail calls basically don't execute any code, they just re-use an existing stack frame
 * and set up a different function and arguments, reallocate locals as needed, and reset the pc to 0.
 * then the execution starts all over again in the same frame
 *
 * TODO: How do I write tests for this?
 * `(fib 1000000)` smashes my default stack size (8mb on macos) without tail calls, so it validates that the
 * tail calls are being used, but it is slow (2s) and I'm impatient.
 */

// (8)              | (objectref, args... -> ...)
RetVal tryInvokeDynTailEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  // fail: not all values are invocable
  Invocable invocable;
  throws(tryPopInvocable(vm, frame, &invocable, error));

  throws(replaceFrame(vm, frame, invocable.fnRef, error));
  throws(tryInvokePopulateLocals(vm, frame, frame, invocable, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)              | (objectref ->)
RetVal tryRetEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value v;
  throws(popOperand(frame, &v, error));
  throws(setResult(frame, v, error));
  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> 0 | 1)
RetVal tryCmpEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &a, error));
  throws(popOperand(frame, &b, error));

  Value c;
  c.type = VT_BOOL;
  c.value = false;

  if (a.type == b.type && a.value == b.value) {
    // NOTE: doesn't do equivalence on heap objects, reference identity compared only
    c.value = true;
  }

  throws(pushOperand(frame, c, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (->)
RetVal tryJmpEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t newPc;
  throws(readIndex(frame, &newPc, error));
  throws(setPc(frame, newPc, error));
  return R_SUCCESS;

  failure:
    return ret;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value test;
  throws(popOperand(frame, &test, error));

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

  uint16_t newPc;
  throws(readIndex(frame, &newPc, error));
  if (truthy) {
    throws(setPc(frame, newPc, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfNotEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value test;
  throws(popOperand(frame, &test, error));

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

  uint16_t newPc;
  throws(readIndex(frame, &newPc, error));
  if (!truthy) {
    throws(setPc(frame, newPc, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal tryAddEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &b, error));
  throws(popOperand(frame, &a, error));

  if (a.type != VT_UINT || b.type != VT_UINT) {
    // fail: not all values are addable
    throwRuntimeError(error, "can only add two integers");
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value + b.value;

  throws(pushOperand(frame, c, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal trySubEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &b, error));
  throws(popOperand(frame, &a, error));

  if (a.type != VT_UINT || b.type != VT_UINT) {
    // fail: not all values are subtractable
    throwRuntimeError(error, "can only subtract two integers");
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value - b.value;

  throws(pushOperand(frame, c, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16)  | (value ->)
RetVal tryDefVarEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value value;
  uint16_t constantIndex;
  Value varName;

  throws(popOperand(frame, &value, error));
  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &varName, error));

  String str;
  throws(tryDerefString(&vm->gc, varName, &str, error));
  throws(tryDefVar(&vm->namespaces, str.value, str.length, value, error));

  // define always returns nil
  Value result;
  result.type = VT_NIL;
  result.value = 0;
  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset 16  | (-> value)
RetVal tryLoadVarEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex;
  Value varName;

  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &varName, error));

  String str;
  throws(tryDerefString(&vm->gc, varName, &str, error));

  Var *var;
  if (!resolveVar(&vm->namespaces, str.value, str.length, &var)) {
    // fail: not all vars exist
    throwRuntimeError(error, "no such var found: '%ls'", str.value);
  }
  else {
    throws(pushOperand(frame, var->value, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (captures... -> value)
RetVal tryLoadClosureEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex;
  Value fnValue;

  throws(readIndex(frame, &constantIndex, error));
  throws(getConst(frame, constantIndex, &fnValue, error));

  if (fnValue.type != VT_FN) {
    throwRuntimeError(error, "cannot create a closure from this value type: %u", fnValue.type);
  }

  Fn *fn;
  throws(deref(&vm->gc1, (void*)&fn, fnValue.value, error));

  Closure closure;
  closure.fn = fnValue;
  closure.numCaptures = fn->numCaptures;
  tryMalloc(closure.captures, fn->numCaptures * sizeof(Value), "Value array");

  // pop captures in reverse order, same as arguments
  for (uint16_t i=0; i<closure.numCaptures; i++) {
    Value capture;
    throws(popOperand(frame, &capture, error));
    uint16_t idx = fn->numCaptures - (1 + i);
    closure.captures[idx] = capture;
  }

  Value closureValue;
  throws(tryAllocateClosure(&vm->gc, closure, &closureValue, error));
  throws(pushOperand(frame, closureValue, error));

  return R_SUCCESS;

  failure:
    _closureFreeContents(&closure);
    return ret;
}

// (8)        | (a, b -> b, a)
RetVal trySwapEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(popOperand(frame, &a, error));
  throws(popOperand(frame, &b, error));

  throws(pushOperand(frame, a, error));
  throws(pushOperand(frame, b, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)        | (jumpAddr, handler ->)
RetVal trySetHandlerEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  ExceptionHandler handler;

  throws(readIndex(frame, &handler.jumpAddress, error));
  throws(readIndex(frame, &handler.localIndex, error));

  setHandler(frame, handler);

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)        | (->)
RetVal tryClearHandlerEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  clearHandler(frame);
  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (x, seq -> newseq)
RetVal tryConsEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value x, seq;
  throws(popOperand(frame, &seq, error));
  throws(popOperand(frame, &x, error));

  Value result;
  if (seq.type == VT_NIL || seq.type == VT_LIST) {
    Cons cons;
    cons.value = x;
    cons.next = seq;
    throws(tryAllocateCons(&vm->gc, cons, &result, error));
  }
  else {
    // TODO: we need to print the actual type here, should make a metadata table for value types
    // fail: not all types can be used as a seq
    throwRuntimeError(error, "cannot cons onto a value of type %u", seq.type);
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> x)
RetVal tryFirstEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value seq;
  throws(popOperand(frame, &seq, error));

  Value result;

  if (seq.type == VT_NIL) {
    result = nil();
  }
  else if (seq.type == VT_LIST) {
    Cons cons;
    throws(tryDerefCons(&vm->gc, seq, &cons, error));
    result = cons.value;
  }
  else {
    // TODO: we need to print the actual type here, should make a metadata table for value types
    // fail: not all types can be used as a seq
    throwRuntimeError(error, "cannot get first from a value of type %u", seq.type);
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> seq)
RetVal tryRestEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value seq;
  throws(popOperand(frame, &seq, error));

  Value result;

  if (seq.type == VT_NIL) {
    result = nil();
  }
  else if (seq.type == VT_LIST) {
    Cons cons;
    throws(tryDerefCons(&vm->gc, seq, &cons, error));
    result = cons.next;
  }
  else {
    // fail: not all types can be used as a seq
    // TODO: we need to print the actual type here, should make a metadata table for value types
    throwRuntimeError(error, "cannot get rest from a value of type %u", seq.type);
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (name -> nil)
RetVal trySetMacroEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value strValue;
  throws(popOperand(frame, &strValue, error));

  if (strValue.type != VT_STR) {
    // fail: not all types identify vars
    throwRuntimeError(error, "only symbols can identify vars: %u", strValue.type);
  }

  String str;
  throws(tryDerefString(&vm->gc, strValue, &str, error));

  Var *var;
  if (!resolveVar(&vm->namespaces, str.value, str.length, &var)) {
    // fail: not all vars exist
    throwRuntimeError(error, "no such var exists: %ls", str.value);
  }

  if (!var->isMacro) {
    if (var->value.type != VT_FN) {
      // fail: only vars referring to functions can be macros
      throwRuntimeError(error, "only vars referring to functions can be macros: %ls, %u", str.value, var->value.type);
    }
    var->isMacro = true;
  }

  throws(pushOperand(frame, nil(), error));

  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (name -> bool)
RetVal tryGetMacroEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  Value strValue;
  throws(popOperand(frame, &strValue, error));

  if (strValue.type != VT_STR) {
    // fail: not all types identify vars
    throwRuntimeError(error, "only symbols can identify vars: %u", strValue.type);
  }

  String str;
  throws(tryDerefString(&vm->gc, strValue, &str, error));

  Value result;
  result.type = VT_BOOL;
  result.value = false;

  Var *var;
  if (resolveVar(&vm->namespaces, str.value, str.length, &var)) {
    result.value = var->isMacro;
  }

  throws(pushOperand(frame, result, error));

  return R_SUCCESS;

  failure:
  return ret;
}

void printInst(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\n", *i, name);
}

void printInstAndIndex(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\t%u\n", *i, name, code[*i + 1] << 8 | code[*i + 2]);
  *i = *i + 2;
}

void printInstAndIndex2x(int *i, const char* name, uint8_t *code) {
  uint16_t index1 = code[*i + 1] << 8 | code[*i + 2];
  uint16_t index2 = code[*i + 3] << 8 | code[*i + 4];
  printf("%i:\t%s\t%u, %u\n", *i, name, index1, index2);
  *i = *i + 4;
}

void printUnknown(int *i, const char* name, uint8_t *code) {
  printf("%i:\t<UNKNOWN>/%u\n", *i, code[*i]);
}

InstTable instTableCreate() {
  InstTable table;

  // init table with blanks
  uint16_t instructionsAllocated = sizeof(table.instructions) / sizeof(table.instructions[0]);
  for (int i=0; i<instructionsAllocated; i++) {
    table.instructions[i].name = NULL;
    table.instructions[i].print = NULL;
    table.instructions[i].tryEval = NULL;
  }

  // init with known instructions
  Inst instructions[]      = {
      [I_LOAD_CONST]       = { .name = "I_LOAD_CONST",      .print = printInstAndIndex,   .tryEval = tryLoadConstEval },
      [I_LOAD_LOCAL]       = { .name = "I_LOAD_LOCAL",      .print = printInstAndIndex,   .tryEval = tryLoadLocalEval },
      [I_STORE_LOCAL]      = { .name = "I_STORE_LOCAL",     .print = printInstAndIndex,   .tryEval = tryStoreLocalEval },
      [I_INVOKE_DYN]       = { .name = "I_INVOKE_DYN",      .print = printInst,           .tryEval = tryInvokeDynEval },
      [I_INVOKE_DYN_TAIL]  = { .name = "I_INVOKE_DYN_TAIL", .print = printInst,           .tryEval = tryInvokeDynTailEval },
      [I_RET]              = { .name = "I_RET",             .print = printInst,           .tryEval = tryRetEval },
      [I_CMP]              = { .name = "I_CMP",             .print = printInst,           .tryEval = tryCmpEval },
      [I_JMP]              = { .name = "I_JMP",             .print = printInstAndIndex,   .tryEval = tryJmpEval },
      [I_JMP_IF]           = { .name = "I_JMP_IF",          .print = printInstAndIndex,   .tryEval = tryJmpIfEval },
      [I_JMP_IF_NOT]       = { .name = "I_JMP_IF_NOT",      .print = printInstAndIndex,   .tryEval = tryJmpIfNotEval },
      [I_ADD]              = { .name = "I_ADD",             .print = printInst,           .tryEval = tryAddEval },
      [I_SUB]              = { .name = "I_SUB",             .print = printInst,           .tryEval = trySubEval },
      [I_DEF_VAR]          = { .name = "I_DEF_VAR",         .print = printInstAndIndex,   .tryEval = tryDefVarEval },
      [I_LOAD_VAR]         = { .name = "I_LOAD_VAR",        .print = printInstAndIndex,   .tryEval = tryLoadVarEval },
      [I_LOAD_CLOSURE]     = { .name = "I_LOAD_CLOSURE",    .print = printInstAndIndex,   .tryEval = tryLoadClosureEval },
      [I_SWAP]             = { .name = "I_SWAP",            .print = printInst,           .tryEval = trySwapEval },
      [I_SET_HANDLER]      = { .name = "I_SET_HANDLER",     .print = printInstAndIndex2x, .tryEval = trySetHandlerEval },
      [I_CLEAR_HANDLER]    = { .name = "I_CLEAR_HANDLER",   .print = printInst,           .tryEval = tryClearHandlerEval },

      [I_CONS]             = { .name = "I_CONS",            .print = printInst,           .tryEval = tryConsEval },
      [I_FIRST]            = { .name = "I_FIRST",           .print = printInst,           .tryEval = tryFirstEval},
      [I_REST]             = { .name = "I_REST",            .print = printInst,           .tryEval = tryRestEval },
      [I_SET_MACRO]        = { .name = "I_SET_MACRO",       .print = printInst,           .tryEval = trySetMacroEval},
      [I_GET_MACRO]        = { .name = "I_GET_MACRO",       .print = printInst,           .tryEval = tryGetMacroEval},


//      [I_NEW]         = { .name = "I_NEW",         .print = printUnknown},
//      [I_GET_FIELD]   = { .name = "I_GET_FIELD",   .print = printUnknown},
//      [I_SET_FIELD]   = { .name = "I_SET_FIELD",   .print = printUnknown},
//      [I_LOAD_ARRAY]  = { .name = "I_LOAD_ARRAY",  .print = printUnknown},
//      [I_STORE_ARRAY] = { .name = "I_STORE_ARRAY", .print = printUnknown},
// requires garbage collection
//      case I_NEW:           // (8), objlen (16) | (-> objectref)
//      case I_GET_FIELD:     // (8), typeIndex  (16) | (objectref -> value)
//      case I_SET_FIELD:     // (8), typeIndex  (16) | (objectref, value ->)
//      case I_NEW_ARRAY:     // (8), objlen (16) | (arraylen -> objectref)
//      case I_LOAD_ARRAY:    // (8)              | (objectref, typeIndex -> value)
//      case I_STORE_ARRAY:   // (8)              | (objectref, typeIndex, value ->)
  };
  memcpy(table.instructions, instructions, sizeof(instructions));
  table.numInstructions = sizeof(instructions) / sizeof(instructions[0]);

  return table;
}

/*
 * code printing utils based on InstTable metadata
 */

const char* getInstName(InstTable *instTable, uint8_t inst) {
  return instTable->instructions[inst].name;
}

void _printCodeArray(InstTable *table, uint8_t *code, uint16_t codeLength) {
  for (int i=0; i<codeLength; i++) {
    Inst inst = table->instructions[code[i]];
    inst.print(&i, inst.name, code);
  }
}

void _printFnConstant(InstTable *table, FnConstant fnConst) {

  for (uint16_t i=0; i<fnConst.numConstants; i++) {
    Constant c = fnConst.constants[i];
    if (c.type == CT_FN) {
      printf("constant fn within constant fn %u:\n", i);
      _printFnConstant(table, c.function);
    }
  }

  printf("fn const code:\n");
  _printCodeArray(table, fnConst.code.code, fnConst.code.codeLength);
}

void _printCodeUnit(InstTable *table, CodeUnit *unit) {

  for (uint16_t i=0; i<unit->numConstants; i++) {
    Constant c = unit->constants[i];
    if (c.type == CT_FN) {
      printf("constant fn %u:\n", i);
      _printFnConstant(table, c.function);
    }
  }

  printf("code:\n");
  _printCodeArray(table, unit->code.code, unit->code.codeLength);
}

void printEvalError(ExecFrame_t frame, Error *error) {

  printf("unhandled error: %ls", error->message);

  ExecFrame_t current = frame;
  while (true) {

    wchar_t *fnName;
    if (hasFnName(current)) {
      Text text;
      textInitContents(&text);
      if (getFnName(current, &text, error) != R_SUCCESS) {
        break;
      }
      fnName = text.value;
    }
    else {
      fnName = L"<root>";
    }

    wchar_t *file = L"core.lsp";
    uint16_t lineNo = 100;
    printf("\t%ls(%ls:%u)\n", fnName, file, lineNo);

    if (!hasParent(current)) {
      break;
    }
    else {
      if (getParent(current, &current, error) != R_SUCCESS) {
        break;
      }
    }
  }
}

RetVal tryExceptionMake(ExecFrame_t frame, VMException *exception, Error *error) {
  RetVal ret;

  Error reference = *error;

  wchar_t msg[ERROR_MSG_LENGTH];
  exceptionInitContents(exception);

  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", reference.message);
  throws(tryTextMake(msg, &exception->message, wcslen(msg), error));

  uint64_t numFrames = 0;
  {
    ExecFrame_t current = frame;
    while (true) {
      numFrames++;
      if (!hasParent(current)) {
        break;
      }
      else {
        throws(getParent(current, &current, error) != R_SUCCESS);
      }
    }
  }

  // native frame
  numFrames++;

  exception->frames.length = numFrames;
  tryMalloc(exception->frames.elements, sizeof(VMExceptionFrame) * numFrames, "VMExceptionFrame array");

  { // native frame

    VMExceptionFrame *f = &exception->frames.elements[0];
    exFrameInitContents(f);

    f->functionName.length = strlen(reference.functionName) + 1;
    tryMalloc(f->functionName.value, f->functionName.length * sizeof(wchar_t), "wide string");
    swprintf(f->functionName.value, f->functionName.length, L"%s", reference.functionName);

    f->unknownSource = false;

    char* fileName = basename((char *) reference.fileName);
    f->fileName.length = strlen(fileName) + 1;
    tryMalloc(f->fileName.value, f->fileName.length * sizeof(wchar_t), "wide string");
    swprintf(f->fileName.value, f->fileName.length, L"%s", fileName);

    f->lineNumber = reference.lineNumber;
  }

  ExecFrame_t current = frame;
  for (uint64_t i=1; i<numFrames; i++) {

    VMExceptionFrame *f = &exception->frames.elements[i];
    exFrameInitContents(f);

    if (hasFnName(current)) {
      Text text;
      throws(getFnName(current, &text, error));
      throws(tryTextCopy(&text, &f->functionName, error));
    }
    else {
      wchar_t *name = L"<root>\0";
      throws(tryTextMake(name, &f->functionName, wcslen(name), error));
    }

    if (hasSourceTable(current)) {
      f->unknownSource = false;
      getFileName(current, &f->fileName);
      getLineNumber(current, &f->lineNumber);
    }

    if (hasParent(current)) {
      throws(getParent(current, &current, error));
    }
  }

  return R_SUCCESS;

  failure:
    exceptionFreeContents(exception);
    return ret;
}

RetVal tryExceptionPrint(VMException *e, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(&b, error));

  throws(tryStringBufferAppendStr(b, error->message, error));

  wchar_t msg[ERROR_MSG_LENGTH];

  for (uint64_t i=0; i<e->frames.length; i++) {
    VMExceptionFrame *f = &e->frames.elements[i];

    if (f->unknownSource) {
      swprintf(msg, ERROR_MSG_LENGTH, L"\t%at ls(Unknown Source)\n", f->functionName.value);
    }
    else {
      swprintf(msg, ERROR_MSG_LENGTH, L"\t%at ls(%ls:%llu)\n", f->functionName.value, f->fileName.value, f->lineNumber);
    }
    throws(tryStringBufferAppendStr(b, msg, error));
  }

  wchar_t *output;
  throws(tryCopyText(stringBufferText(b), &output, stringBufferLength(b), error));
  stringBufferFree(b);

  *ptr = output;
  return R_SUCCESS;

  failure:
  stringBufferFree(b);
  return ret;
}

RetVal tryExceptionPrintf(VMException *e, Error *error) {
  RetVal ret;

  wchar_t *msg;
  throws(tryExceptionPrint(e, &msg, error));
  printf("%ls\n", msg);
  free(msg);

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryFrameEval(VM *vm, ExecFrame_t frame, Error *error) {
  RetVal ret;

  uint8_t inst;
  TryEval tryEval;

  while (true) {

    if (hasResult(frame)) {
      if (!hasParent(frame)) {
        break;
      }
      else {
        Value result;
        ExecFrame_t parent;

        throws(getResult(frame, &result, error));
        throws(getParent(frame, &parent, error));
        throws(pushOperand(parent, result, error));

        popFrame(frame);
      }
    }

    throws(readInstruction(frame, &inst, error));
    tryEval = vm->instTable.instructions[inst].tryEval;

    if (tryEval == NULL) {
      throwRuntimeError(error, "instruction unimplemented: %s (%u)", getInstName(&vm->instTable, inst), inst);
    }

    ret = tryEval(vm, frame, error);

    if (ret != R_SUCCESS) {
      VMException ex;
      throws(tryExceptionMake(frame, &ex, error));
      setException(frame, ex);
      break;
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

  // create an exception
  // - allocate it
  // - set its payload with the error message
  // - initialize its stack trace information
  // - the entire exception is just lists of lists (payload, trace)
  // walk up the stack until we find an error handler
  // if no error handler is found, print the stack trace and goto failure
  // if an error handler is found
  // - pop the stack until the frame with the error handler is the current frame
  // - set the exception as the local index for the handler's exception binding
  // - jump to the error handler's jump address

  /*
   * TODO: how exception handling should work, revised
   * - analyzer defines a try/catch form, where the catch introduces a binding in the binding table + some forms to eval
   * - compiler computes the jumpAddress for the catch block
   * - compiler emits I_SET_HANDLER with 2 index parameters (jumpAddress and exceptionLocalIndex)
   * - vm makes use of error handlers *here* as needed
   *
   * this means no need for lambda/closure execution at error handling time
   */

/*
 * The OpStack
 */

void opStackInitContents(OpStack *stack) {
  stack->usedDepth = 0;
  stack->maxDepth = 0;
  stack->stack = NULL;
}

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

/*
 * ExecFrame Implementation
 */

typedef struct ExecFrame ExecFrame;

typedef struct ExecFrame {
  ExecFrame *parent;

  Value fnRef;
  Fn *fn; // TODO: the collector will have to treat relocating functions specially to make this reference work

  Value *locals;
  OpStack *opStack;
  Value result;
  bool resultAvailable;
  uint16_t pc;

  ExceptionHandler handler;
  bool handlerSet;

  VMException exception;
  bool exceptionSet;
} ExecFrame;

void handlerInitContents(ExceptionHandler *h) {
  h->localIndex = 0;
  h->jumpAddress = 0;
}

void frameInitContents(ExecFrame *frame) {
  frame->parent = NULL;
  frame->fnRef = nil();
  frame->fn = NULL;
  frame->locals = NULL;
  frame->opStack = NULL;
  frame->resultAvailable = 0;
  frame->result.type = VT_NIL;
  frame->result.value = 0;
  frame->pc = 0;
  handlerInitContents(&frame->handler);
  frame->handlerSet = false;
  exceptionInitContents(&frame->exception);
  frame->exceptionSet = false;
}

RetVal readInstruction(ExecFrame *frame, uint8_t *ptr, Error *error) {
  RetVal ret;

  if (frame->pc >= frame->fn->codeLength) {
    throwRuntimeError(error, "cannot read next instruction, no instructions left");
  }

  *ptr = frame->fn->code[frame->pc];
  frame->pc += 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal readIndex(ExecFrame *frame, uint16_t *ptr, Error *error) {
  RetVal ret;

  if (frame->pc + 1 >= frame->fn->codeLength) {
    throwRuntimeError(error, "cannot read next instruction, no instructions left");
  }

  uint8_t *code = frame->fn->code;
  uint16_t pc = frame->pc;
  *ptr = (code[pc] << 8) | code[pc + 1];
  frame->pc += 2;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal setPc(ExecFrame *frame, uint16_t newPc, Error *error) {
  RetVal ret;

  if (newPc >= frame->fn->codeLength) {
    throwRuntimeError(error, "no such instruction: %u", newPc);
  }

  frame->pc = newPc;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getConst(ExecFrame *frame, uint16_t constantIndex, Value *ptr, Error *error) {
  RetVal ret;

  if (constantIndex >= frame->fn->numConstants) {
    throwRuntimeError(error, "no such constant: %u", constantIndex);
  }

  *ptr = frame->fn->constants[constantIndex];
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getLocal(ExecFrame *frame, uint16_t localIndex, Value *ptr, Error *error) {
  RetVal ret;

  if (localIndex >= frame->fn->numLocals) {
    throwRuntimeError(error, "no such local: %u", localIndex);
  }

  *ptr = frame->locals[localIndex];
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal setLocal(ExecFrame *frame, uint16_t localIndex, Value value, Error *error) {
  RetVal ret;

  if (localIndex >= frame->fn->numLocals) {
    throwRuntimeError(error, "no such local: %u", localIndex);
  }

  frame->locals[localIndex] = value;
  return R_SUCCESS;

  failure:
  return ret;
}

uint16_t pushOperand(ExecFrame *frame, Value value, Error *error) {
  RetVal ret;

  throws(tryOpStackPush(frame->opStack, value, error));
  return R_SUCCESS;

  failure:
  return ret;
}

uint16_t popOperand(ExecFrame *frame, Value *value, Error *error) {
  RetVal ret;

  throws(tryOpStackPop(frame->opStack, value, error));
  return R_SUCCESS;

  failure:
  return ret;
}

bool hasResult(ExecFrame *frame) {
  return frame->resultAvailable;
}

bool hasParent(ExecFrame *frame) {
  return frame->parent != NULL;
}

RetVal getParent(ExecFrame *frame, ExecFrame **ptr, Error *error) {
  RetVal ret;

  if (frame->parent == NULL) {
    throwRuntimeError(error, "no parent available");
  }

  *ptr = frame->parent;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal setResult(ExecFrame *frame, Value result, Error *error) {
  RetVal ret;

  if (frame->resultAvailable) {
    throwRuntimeError(error, "result already set");
  }

  frame->result = result;
  frame->resultAvailable = true;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal getResult(ExecFrame *frame, Value *ptr, Error *error) {
  RetVal ret;

  if (!frame->resultAvailable) {
    throwRuntimeError(error, "result not set");
  }

  *ptr = frame->result;
  return R_SUCCESS;

  failure:
  return ret;
}

bool hasHandler(ExecFrame *frame) {
  return frame->handlerSet;
}

RetVal getHandler(ExecFrame_t frame, ExceptionHandler *ptr, Error *error) {
  RetVal ret;

  if (!frame->handlerSet) {
    throwRuntimeError(error, "handler not set");
  }

  *ptr = frame->handler;
  return R_SUCCESS;

  failure:
    return ret;
}

void setHandler(ExecFrame_t frame, ExceptionHandler handler) {
  frame->handler = handler;
  frame->handlerSet = true;
}

void clearHandler(ExecFrame_t frame) {
  handlerInitContents(&frame->handler);
  frame->handlerSet = false;
}

bool hasFnName(ExecFrame *frame) {
  return frame->fn->hasName;
}

RetVal getFnName(ExecFrame_t frame, Text *name, Error *error) {
  RetVal ret;

  if (!frame->fn->hasName) {
    throwRuntimeError(error, "no fn name found");
  }

  name->value = frame->fn->name;
  name->length = frame->fn->nameLength;
  return R_SUCCESS;

  failure:
    return ret;
}

bool hasSourceTable(ExecFrame *frame) {
  return frame->fn->hasSourceTable;
}

bool getLineNumber(ExecFrame *frame, uint64_t *lineNumber) {
  if (frame->fn->hasSourceTable) {
    for (uint64_t i=0; i<frame->fn->numLineNumbers; i++) {
      LineNumber *l = &frame->fn->lineNumbers[i];
      if (l->startInstructionIndex >= frame->pc) {
        break;
      }
      else {
        *lineNumber = l->lineNumber;
      }
    }
  }
  return false;
}

bool getFileName(ExecFrame_t frame, Text *fileName) {
  if (frame->fn->hasSourceTable) {
    fileName->value = frame->fn->sourceFileName;
    fileName->length = frame->fn->sourceFileNameLength;
    return true;
  }
  return false;
}

bool hasException(ExecFrame_t frame) {
  return frame->exceptionSet;
}

void setException(ExecFrame_t frame, VMException e) {
  frame->exception = e;
  frame->exceptionSet = true;
}

RetVal getException(ExecFrame_t frame, VMException *e, Error *error) {
  RetVal ret;

  if (!frame->exceptionSet) {
    throwRuntimeError(error, "handler not set");
  }

  *e = frame->exception;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal pushFrame(VM *vm, ExecFrame **framePtr, Value newFn, Error *error) {
  RetVal ret;

  Fn *fn = NULL;
  throws(deref(&vm->gc1, (void*)&fn, newFn.value, error));

  // clean up on fail
  ExecFrame_t parent = NULL;

  ExecFrame *frame = *framePtr;
  if (frame == NULL) {
    tryMalloc(frame, sizeof(ExecFrame), "ExecFrame");
  }
  else {
    tryMalloc(parent, sizeof(ExecFrame), "ExecFrame");
    memcpy(parent, frame, sizeof(ExecFrame));
  }

  ExecFrame child;
  frameInitContents(&child);

  child.parent = parent;
  child.fnRef = newFn;
  child.fn = fn;

  tryMalloc(child.opStack, sizeof(OpStack), "OpStack");
  tryMalloc(child.locals, sizeof(Value) * child.fn->numLocals, "Value array");
  throws(tryOpStackInitContents(child.opStack, child.fn->maxOperandStackSize, error));

  *frame = child;
  *framePtr = frame;
  return R_SUCCESS;

  failure:
    if (parent != NULL) {
      free(parent);
    }
    free(child.locals);
    opStackFreeContents(child.opStack);
    return ret;
}

RetVal replaceFrame(VM *vm, ExecFrame *frame, Value newFn, Error *error) {
  RetVal ret;

  Fn *fn = NULL;
  throws(deref(&vm->gc1, (void*)&fn, newFn.value, error));

  // resize locals if needed
  if (fn->numLocals > frame->fn->numLocals) {
    Value *resizedLocals = realloc(frame->locals, fn->numLocals * sizeof(Value));
    if (resizedLocals == NULL) {
      throwMemoryError(error, "realloc Value array");
    }
    frame->locals = resizedLocals;
  }

  if (fn->maxOperandStackSize > frame->opStack->maxDepth) {
    Value *resizedStack = realloc(frame->opStack->stack, fn->maxOperandStackSize * sizeof(Value));
    if (resizedStack == NULL) {
      throwMemoryError(error, "realloc Value array");
    }
    frame->opStack->maxDepth = fn->maxOperandStackSize;
    frame->opStack->stack = resizedStack;
  }

  frame->fnRef = newFn;
  frame->fn = fn;
  frame->result = nil();
  frame->resultAvailable = false;
  frame->pc = 0;

  return R_SUCCESS;

  failure:
    return ret;
}

void popFrame(ExecFrame *frame) {

  ExecFrame *child = frame;
  ExecFrame *parent = frame->parent;

  opStackFreeContents(child->opStack);
  free(child->locals);

  if (parent != NULL) {
    *frame = *parent;
    free(parent);
  }
}

/*
 * The top level frame could just be a regular frame
 * - that has an extra local, which points to a function
 * - which happens to be the function created from the root expression and allocated on the heap
 *
 * This would remove all special cases in stack stuff, except for how that initial function gets hydrated, allocated, and bound
 *
 */

RetVal _tryVMEval(VM *vm, CodeUnit *codeUnit, Value *result, VMException *exception, bool *exceptionThrown,  Error *error) {

  RetVal ret;

  bool pushed = false;

  FnConstant c;
  constantFnInitContents(&c);
  c.numConstants = codeUnit->numConstants;
  c.constants = codeUnit->constants;
  c.code = codeUnit->code;

  Value fnRef = nil();
  throws(tryFnHydrate(vm, &c, &fnRef, error));

  ExecFrame_t frame;
  throws(pushFrame(vm, &frame, fnRef, error));
  pushed = true;

  throws(tryFrameEval(vm, frame, error));

  if (frame->exceptionSet) {
    *exceptionThrown = true;
    *exception = frame->exception;
  }
  else {
    *result = frame->result;
  }

  popFrame(frame);

  return R_SUCCESS;

  failure:
    if (pushed) {
      popFrame(frame);
    }
    return ret;
}

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, VMEvalResult *result, Error *error) {

  RetVal ret;

  Value value;
  VMException exception;
  bool exceptionThrown = false;

  throws(_tryVMEval(vm, codeUnit, &value, &exception, &exceptionThrown, error));

  if (exceptionThrown) {
    result->type = RT_EXCEPTION;
    result->exception = exception;
  }
  else {
    result->type = RT_RESULT;
    throws(tryVMPrn(vm, value, &result->result, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryVMInitContents(VM *vm , Error *error) {
  RetVal ret;

  vm->instTable = instTableCreate();
  GCInit(&vm->gc);
  throws(tryGC1InitContents(&vm->gc1, 1024 * 1000, error));
  throws(tryNamespacesInitContents(&vm->namespaces, error));

  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFreeContents(VM *vm) {
  if (vm != NULL) {
    GCFreeContents(&vm->gc);
    GC1FreeContents(&vm->gc1);
    namespacesFreeContents(&vm->namespaces);
  }
}

RetVal tryVMMake(VM **ptr , Error *error) {
  RetVal ret;

  tryMalloc(*ptr, sizeof(VM), "VM malloc");
  throws(tryVMInitContents(*ptr, error));

  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFree(VM *vm) {
  if (vm != NULL) {
    vmFreeContents(vm);
    free(vm);
  }
}

/*
 * External code printing utilities
 */

void printCodeArray(uint8_t *code, uint16_t codeLength) {
  InstTable table = instTableCreate();
  _printCodeArray(&table, code, codeLength);
}

void printCodeUnit(CodeUnit *unit) {
  InstTable table = instTableCreate();
  _printCodeUnit(&table, unit);
}



