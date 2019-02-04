#include <stdlib.h>
#include <string.h>
#include "vm.h"
#include "utils.h"

/*
 * CodeUnit init/free functions
 */

void sourceTableInitContents(SourceTable *t) {
  t->lineNumbers = NULL;
  t->numLineNumbers = 0;
  t->fileName = NULL;
  t->fileNameLength = 0;
}

void sourceTableFreeContents(SourceTable *t) {
  t->fileNameLength = 0;
  if (t->fileName != NULL) {
    free(t->fileName);
    t->fileName = NULL;
  }
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
    to->sourceTable.fileNameLength = from->sourceTable.fileNameLength;
    throws(tryCopyText(from->sourceTable.fileName, &to->sourceTable.fileName, to->sourceTable.fileNameLength, error));

    to->sourceTable.numLineNumbers = from->sourceTable.numLineNumbers;
    tryMalloc(to->sourceTable.lineNumbers, sizeof(LineNumber) * to->sourceTable.numLineNumbers, "LineNumber array");
  }

  return R_SUCCESS;

  failure:
  codeFreeContents(to);
  return ret;
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
  Text name;
  uint16_t numCaptures;
  uint16_t numArgs;
  bool usesVarArgs;
  uint16_t numConstants;
  Value *constants;
  Code code;
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

typedef struct Cons Cons;

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

typedef struct Frame Frame;

typedef RetVal (*TryEval) (struct VM *vm, Frame *frame, Error *error);

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
  Namespaces namespaces;
  InstTable instTable;
} VM;

// frames

typedef struct OpStack {
  Value *stack;
  uint64_t maxDepth;
  uint64_t usedDepth;
} OpStack;

typedef struct Frame {
  Frame *parent;
  uint16_t numConstants; // TODO: make a verifier so we can check these bounds at load time rather than compile time
  Value *constants;
  Code code;
  uint16_t numLocals;
  Value *locals;
  OpStack *opStack;
  Value result;
  bool resultAvailable;
  uint16_t pc;
} Frame;

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
 * gc/runtime implementation
 */

void GCInit(GC *gc) {

  gc->usedFnSpace = 0;
  gc->allocatedFnSpace = 0;
  gc->fns = NULL;

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

void _fnFreeContents(Fn *fn);
void _stringFreeContents(String *str);
void _symbolFreeContents(Symbol *s);
void _keywordFreeContents(Keyword *k);
void _consFreeContents(Cons *c);

void GCFreeContents(GC *gc) {
  if (gc != NULL) {

    for (uint64_t i=0; i<gc->usedFnSpace; i++) {
      _fnFreeContents(&gc->fns[i]);
    }
    free(gc->fns);
    gc->fns = NULL;
    gc->usedFnSpace = 0;
    gc->allocatedFnSpace = 0;

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

RetVal tryAllocateFn(GC *gc, Fn fn, Value *value, Error *error) {
  RetVal ret;

  if (gc->fns == NULL) {
    uint16_t len = 16;
    tryMalloc(gc->fns, len * sizeof(Fn), "Fn array");
    gc->allocatedFnSpace = len;
  }
  else if (gc->usedFnSpace == gc->allocatedFnSpace) {
    uint64_t newAllocatedLength = gc->allocatedFnSpace * 2;

    Fn* resizedFns = realloc(gc->fns, newAllocatedLength * sizeof(Fn));
    if (resizedFns == NULL) {
      ret = memoryError(error, "realloc Fn array");
      goto failure;
    }

    gc->allocatedFnSpace = newAllocatedLength;
    gc->fns = resizedFns;
  }

  uint64_t index = gc->usedFnSpace;
  gc->fns[index] = fn;
  gc->usedFnSpace = index + 1;

  value->type = VT_FN;
  value->value = index;

  return R_SUCCESS;

  failure:
  return ret;
}

void _fnFreeContents(Fn *fn) {
  if (fn != NULL) {
    fn->numArgs = 0;

    fn->numConstants = 0;
    if (fn->constants != NULL) {
      free(fn->constants);
      fn->constants = NULL;
    }

    codeFreeContents(&fn->code);
  }
}

RetVal tryDerefFn(GC *gc, Value value, Fn *fn, Error *error) {
  RetVal ret;

  if (gc->usedFnSpace <= value.value) {
    throwInternalError(error, "fn reference points to fn that does not exist");
  }

  *fn = gc->fns[value.value];
  return R_SUCCESS;

  failure:
  return ret;
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
 * Instruction Definitions
 */

void printInst(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\n", *i, name);
}

void printInstAndIndex(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\t%u\n", *i, name, code[*i + 1] << 8 | code[*i + 2]);
  *i = *i + 2;
}

void printUnknown(int *i, const char* name, uint8_t *code) {
  printf("%i:\t<UNKNOWN>/%u\n", *i, code[*i]);
}

RetVal tryFrameEval(VM *vm, Frame *frame, Error *error);

uint16_t readIndex(uint8_t *code, uint16_t pc) {
  return (code[pc + 1] << 8) | code[pc + 2];
}

// (8), typeIndex (16) | (-> value)
RetVal tryLoadConstEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex = readIndex(frame->code.code, frame->pc);
  Value constant = frame->constants[constantIndex];
  throws(tryOpStackPush(frame->opStack, constant, error));

  frame->pc = frame->pc + 3;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8), typeIndex (16) | (-> value)
RetVal tryLoadLocalEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  uint16_t localIndex = readIndex(frame->code.code, frame->pc);
  Value v = frame->locals[localIndex];
  throws(tryOpStackPush(frame->opStack, v, error));

  frame->pc = frame->pc + 3;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8), typeIndex  (16) | (objectref ->)
RetVal tryStoreLocalEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  uint16_t localIndex = readIndex(frame->code.code, frame->pc);
  Value v;
  throws(tryOpStackPop(frame->opStack, &v, error));
  frame->locals[localIndex] = v;

  frame->pc = frame->pc + 3;
  return R_SUCCESS;

  failure:
  return ret;
}

void frameInitContents(Frame *frame) {
  frame->parent = NULL;
  frame->numConstants = 0;
  frame->constants = NULL;
  codeInitContents(&frame->code);
  frame->numLocals = 0;
  frame->locals = NULL;
  frame->opStack = NULL;
  frame->resultAvailable = 0;
  frame->result.type = VT_NIL;
  frame->result.value = 0;
  frame->pc = 0;
}

typedef struct Invocable {
  Fn fn;
  bool hasClosure;
  Closure closure;
} Invocable;

RetVal tryPopInvocable(VM *vm, Frame *frame, Invocable *invocable, Error *error) {
  RetVal ret;

  Value popVal;
  throws(tryOpStackPop(frame->opStack, &popVal, error));

  switch (popVal.type) {
    case VT_FN: {
      throws(tryDerefFn(&vm->gc, popVal, &invocable->fn, error));
      invocable->hasClosure = false;
      invocable->closure.fn = nil();
      invocable->closure.numCaptures = 0;
      invocable->closure.captures = NULL;
      break;
    }
    case VT_CLOSURE: {
      throws(tryDerefClosure(&vm->gc, popVal, &invocable->closure, error));
      invocable->hasClosure = true;
      throws(tryDerefFn(&vm->gc, invocable->closure.fn, &invocable->fn, error));
      break;
    }
    default:
      throwRuntimeError(error, "cannot invoke this value type as a function: %u", popVal.type);
  }

  return R_SUCCESS;

  failure:
  return ret;
}

//* - vm honors var-arg flag
//*   - sees var-arg flag on invocable
//*   - pops all static arguments into local slot
//*   - pops number indicating number of extra arguments
//      TODO: do we have to start *aways* passing the number of arguments?
//*   - pops number of extra arguments into a list, sets as final argument in local slot

RetVal tryInvokePopulateLocals(VM *vm, Frame *parent, Frame *child, Invocable invocable, Error *error) {
  RetVal ret;

  Value numArgsSupplied;
  throws(tryOpStackPop(parent->opStack, &numArgsSupplied, error));

  if (numArgsSupplied.type != VT_UINT) {
    throwRuntimeError(error, "first argument must be number of arguments supplied: %u", numArgsSupplied.type);
  }

  if (numArgsSupplied.value > invocable.fn.numArgs) {

    if (!invocable.fn.usesVarArgs) {
      throwRuntimeError(error, "extra arguments supplied, expected %u but got %llu", invocable.fn.numArgs,
          numArgsSupplied.value);
    }

    // read the extra args into a list, push it back on the stack

    Value seq = nil();
    uint16_t numVarArgs = (numArgsSupplied.value - invocable.fn.numArgs) + 1;
    for (uint16_t i = 0; i < numVarArgs; i++) {

      Value arg;
      throws(tryOpStackPop(parent->opStack, &arg, error));

      Cons cons;
      cons.value = arg;
      cons.next = seq;
      throws(tryAllocateCons(&vm->gc, cons, &seq, error));
    }

    throws(tryOpStackPush(parent->opStack, seq, error));
  }

  if (numArgsSupplied.value == invocable.fn.numArgs && invocable.fn.usesVarArgs) {
    // wrap the last arg in a list

    Value arg;
    throws(tryOpStackPop(parent->opStack, &arg, error));

    Value seq = nil();

    Cons cons;
    cons.value = arg;
    cons.next = nil();
    throws(tryAllocateCons(&vm->gc, cons, &seq, error));

    throws(tryOpStackPush(parent->opStack, seq, error));
  }

  if (numArgsSupplied.value < invocable.fn.numArgs) {

    if (!invocable.fn.usesVarArgs) {
      throwRuntimeError(error, "required arguments not supplied, expected %u but got %llu", invocable.fn.numArgs,
                        numArgsSupplied.value);
    }

    // make sure the list is present on the stack

    throws(tryOpStackPush(parent->opStack, nil(), error));
  }

  for (uint16_t i = 0; i < invocable.fn.numArgs; i++) {
    Value arg;
    throws(tryOpStackPop(parent->opStack, &arg, error));

//    if (wcscmp(invocable.fn.name.value, L"reverse") == 0
//        || wcscmp(invocable.fn.name.value, L"concat-two") == 0
//        || wcscmp(invocable.fn.name.value, L"concat") == 0
//        ) {
//      throws(tryVMPrn(vm, arg, error));
//    }

    uint16_t idx = invocable.fn.numArgs - (1 + i);
    child->locals[idx] = arg;
  }

  if (invocable.fn.numCaptures > 0) {

    if (!invocable.hasClosure) {
      throwRuntimeError(error, "cannot invoke this fn without a closure, it captures variables: %u", invocable.fn.numCaptures);
    }
    if (invocable.closure.numCaptures < invocable.fn.numCaptures) {
      throwRuntimeError(error, "closure does not have enough captured variables: %u", invocable.closure.numCaptures);
    }

    uint16_t nextLocalIdx = invocable.fn.numArgs;
    for (uint16_t i=0; i<invocable.fn.numCaptures; i++) {
      child->locals[nextLocalIdx] = invocable.closure.captures[i];
      nextLocalIdx = nextLocalIdx + 1;
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (objectref, args... -> ...)
RetVal tryInvokeDynEval(VM *vm, Frame *frame, Error *error) {

  RetVal ret;
  Invocable invocable;

  // clean up on return
  Frame child;
  frameInitContents(&child);

  throws(tryPopInvocable(vm, frame, &invocable, error));

  child.parent = frame;
  child.numConstants = invocable.fn.numConstants;
  child.constants = invocable.fn.constants;
  child.code = invocable.fn.code;
  child.numLocals = invocable.fn.code.numLocals;

  tryMalloc(child.locals, sizeof(Value) * invocable.fn.code.numLocals, "Value array");

  OpStack childOpStack;
  tryOpStackInitContents(&childOpStack, invocable.fn.code.maxOperandStackSize, error);
  child.opStack = &childOpStack;

  throws(tryInvokePopulateLocals(vm, frame, &child, invocable, error));
  throws(tryFrameEval(vm, &child, error));
  throws(tryOpStackPush(frame->opStack, child.result, error));

  frame->pc = frame->pc + 1;

  ret = R_SUCCESS;
  goto done;

  failure:
    goto done;

  done:
    free(child.locals);
    opStackFreeContents(child.opStack);
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
RetVal tryInvokeDynTailEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Invocable invocable;
  throws(tryPopInvocable(vm, frame, &invocable, error));

  frame->numConstants = invocable.fn.numConstants;
  frame->constants = invocable.fn.constants;
  frame->code = invocable.fn.code;

  // resize locals if needed
  if (invocable.fn.code.numLocals > frame->numLocals) {
    Value *resizedLocals = realloc(frame->locals, invocable.fn.code.numLocals * sizeof(Value));
    if (resizedLocals == NULL) {
      ret = memoryError(error, "realloc Value array");
      goto failure;
    }
    frame->numLocals = invocable.fn.code.numLocals;
    frame->locals = resizedLocals;
  }

  throws(tryInvokePopulateLocals(vm, frame, frame, invocable, error));

  frame->result = nil();
  frame->resultAvailable = false;
  frame->pc = 0;

  return R_SUCCESS;

  failure:
    return ret;
}

// (8)              | (objectref ->)
RetVal tryRetEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value v;
  throws(tryOpStackPop(frame->opStack, &v, error));

  frame->result = v;
  frame->resultAvailable = true;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> 0 | 1)
RetVal tryCmpEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(tryOpStackPop(frame->opStack, &a, error));
  throws(tryOpStackPop(frame->opStack, &b, error));

  Value c;
  c.type = VT_BOOL;
  c.value = false;

  if (a.type == b.type && a.value == b.value) {
    // NOTE: doesn't do equivalence on heap objects, reference identity compared only
    c.value = true;
  }

  throws(tryOpStackPush(frame->opStack, c, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (->)
RetVal tryJmpEval(VM *vm, Frame *frame, Error *error) {
  uint16_t newPc = readIndex(frame->code.code, frame->pc);
  frame->pc = newPc;
  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value test;
  throws(tryOpStackPop(frame->opStack, &test, error));

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
    uint16_t newPc = readIndex(frame->code.code, frame->pc);
    frame->pc = newPc;
  }
  else {
    frame->pc = frame->pc + 3;
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (value ->)
RetVal tryJmpIfNotEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value test;
  throws(tryOpStackPop(frame->opStack, &test, error));

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

  if (!truthy) {
    uint16_t newPc = readIndex(frame->code.code, frame->pc);
    frame->pc = newPc;
  }
  else {
    frame->pc = frame->pc + 3;
  }

  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal tryAddEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(tryOpStackPop(frame->opStack, &b, error));
  throws(tryOpStackPop(frame->opStack, &a, error));

  if (a.type != VT_UINT || b.type != VT_UINT) {

    throwRuntimeError(error, "can only add two integers");
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value + b.value;

  throws(tryOpStackPush(frame->opStack, c, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8)              | (a, b -> c)
RetVal trySubEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(tryOpStackPop(frame->opStack, &b, error));
  throws(tryOpStackPop(frame->opStack, &a, error));

  if (a.type != VT_UINT || b.type != VT_UINT) {

    throwRuntimeError(error, "can only add two integers");
  }

  Value c;
  c.type = VT_UINT;
  c.value = a.value - b.value;

  throws(tryOpStackPush(frame->opStack, c, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16)  | (value ->)
RetVal tryDefVarEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value value;
  throws(tryOpStackPop(frame->opStack, &value, error));

  uint16_t constantIndex = readIndex(frame->code.code, frame->pc);
  Value varName = frame->constants[constantIndex];

  String str;
  throws(tryDerefString(&vm->gc, varName, &str, error));
  throws(tryDefVar(&vm->namespaces, str.value, str.length, value, error));

  // define always returns nil
  Value result;
  result.type = VT_NIL;
  result.value = 0;
  throws(tryOpStackPush(frame->opStack, result, error));

  frame->pc = frame->pc + 3;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset 16  | (-> value)
RetVal tryLoadVarEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex = readIndex(frame->code.code, frame->pc);
  Value varName = frame->constants[constantIndex];

  String str;
  throws(tryDerefString(&vm->gc, varName, &str, error));

  Var *var;
  if (!resolveVar(&vm->namespaces, str.value, str.length, &var)) {
    throwRuntimeError(error, "no such var found: '%ls'", str.value);
  }
  else {
    throws(tryOpStackPush(frame->opStack, var->value, error));
  }

  frame->pc = frame->pc + 3;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8), offset (16) | (captures... -> value)
RetVal tryLoadClosureEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  uint16_t constantIndex = readIndex(frame->code.code, frame->pc);
  Value fnValue = frame->constants[constantIndex];

  if (fnValue.type != VT_FN) {
    throwRuntimeError(error, "cannot create a closure from this value type: %u", fnValue.type);
  }

  Fn fn;
  throws(tryDerefFn(&vm->gc, fnValue, &fn, error));

  Closure closure;
  closure.fn = fnValue;
  closure.numCaptures = fn.numCaptures;
  tryMalloc(closure.captures, fn.numCaptures * sizeof(Value), "Value array");

  // pop captures in reverse order, same as arguments
  for (uint16_t i=0; i<closure.numCaptures; i++) {
    Value capture;
    throws(tryOpStackPop(frame->opStack, &capture, error));
    uint16_t idx = fn.numCaptures - (1 + i);
    closure.captures[idx] = capture;
  }

  Value closureValue;
  throws(tryAllocateClosure(&vm->gc, closure, &closureValue, error));
  throws(tryOpStackPush(frame->opStack, closureValue, error));

  frame->pc = frame->pc + 3;
  return R_SUCCESS;

  failure:
    _closureFreeContents(&closure);
  return ret;
}

// (8)        | (a, b -> b, a)
RetVal trySwapEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value a, b;
  throws(tryOpStackPop(frame->opStack, &a, error));
  throws(tryOpStackPop(frame->opStack, &b, error));

  throws(tryOpStackPush(frame->opStack, a, error));
  throws(tryOpStackPush(frame->opStack, b, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (x, seq -> newseq)
RetVal tryConsEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value x, seq;
  throws(tryOpStackPop(frame->opStack, &seq, error));
  throws(tryOpStackPop(frame->opStack, &x, error));

  Value result;
  if (seq.type == VT_NIL || seq.type == VT_LIST) {
    Cons cons;
    cons.value = x;
    cons.next = seq;
    throws(tryAllocateCons(&vm->gc, cons, &result, error));
  }
  else {
    // TODO: we need to print the actual type here, should make a metadata table for value types
    throwRuntimeError(error, "cannot cons onto a value of type %u", seq.type);
  }

  throws(tryOpStackPush(frame->opStack, result, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> x)
RetVal tryFirstEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value seq;
  throws(tryOpStackPop(frame->opStack, &seq, error));

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
    throwRuntimeError(error, "cannot get first from a value of type %u", seq.type);
  }

  throws(tryOpStackPush(frame->opStack, result, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (seq -> seq)
RetVal tryRestEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value seq;
  throws(tryOpStackPop(frame->opStack, &seq, error));

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
    // TODO: we need to print the actual type here, should make a metadata table for value types
    throwRuntimeError(error, "cannot get rest from a value of type %u", seq.type);
  }

  throws(tryOpStackPush(frame->opStack, result, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
    return ret;
}

// (8),             | (name -> nil)
RetVal trySetMacroEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value strValue;
  throws(tryOpStackPop(frame->opStack, &strValue, error));

  if (strValue.type != VT_STR) {
    throwRuntimeError(error, "only symbols can identify vars: %u", strValue.type);
  }

  String str;
  throws(tryDerefString(&vm->gc, strValue, &str, error));

  Var *var;
  if (!resolveVar(&vm->namespaces, str.value, str.length, &var)) {
    throwRuntimeError(error, "no such var exists: %ls", str.value);
  }

  if (!var->isMacro) {
    if (var->value.type != VT_FN) {
      throwRuntimeError(error, "only vars referring to functions can be macros: %ls, %u", str.value, var->value.type);
    }
    var->isMacro = true;
  }

  throws(tryOpStackPush(frame->opStack, nil(), error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
  return ret;
}

// (8),             | (name -> bool)
RetVal tryGetMacroEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  Value strValue;
  throws(tryOpStackPop(frame->opStack, &strValue, error));

  if (strValue.type != VT_STR) {
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

  throws(tryOpStackPush(frame->opStack, result, error));

  frame->pc = frame->pc + 1;
  return R_SUCCESS;

  failure:
  return ret;
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
      [I_LOAD_CONST]       = { .name = "I_LOAD_CONST",      .print = printInstAndIndex,  .tryEval = tryLoadConstEval },
      [I_LOAD_LOCAL]       = { .name = "I_LOAD_LOCAL",      .print = printInstAndIndex,  .tryEval = tryLoadLocalEval },
      [I_STORE_LOCAL]      = { .name = "I_STORE_LOCAL",     .print = printInstAndIndex,  .tryEval = tryStoreLocalEval },
      [I_INVOKE_DYN]       = { .name = "I_INVOKE_DYN",      .print = printInst,          .tryEval = tryInvokeDynEval },
      [I_INVOKE_DYN_TAIL]  = { .name = "I_INVOKE_DYN_TAIL", .print = printInst,          .tryEval = tryInvokeDynTailEval },
      [I_RET]              = { .name = "I_RET",             .print = printInst,          .tryEval = tryRetEval },
      [I_CMP]              = { .name = "I_CMP",             .print = printInst,          .tryEval = tryCmpEval },
      [I_JMP]              = { .name = "I_JMP",             .print = printInstAndIndex,  .tryEval = tryJmpEval },
      [I_JMP_IF]           = { .name = "I_JMP_IF",          .print = printInstAndIndex,  .tryEval = tryJmpIfEval },
      [I_JMP_IF_NOT]       = { .name = "I_JMP_IF_NOT",      .print = printInstAndIndex,  .tryEval = tryJmpIfNotEval },
      [I_ADD]              = { .name = "I_ADD",             .print = printInst,          .tryEval = tryAddEval },
      [I_SUB]              = { .name = "I_SUB",             .print = printInst,          .tryEval = trySubEval },
      [I_DEF_VAR]          = { .name = "I_DEF_VAR",         .print = printInstAndIndex,  .tryEval = tryDefVarEval },
      [I_LOAD_VAR]         = { .name = "I_LOAD_VAR",        .print = printInstAndIndex,  .tryEval = tryLoadVarEval },
      [I_LOAD_CLOSURE]     = { .name = "I_LOAD_CLOSURE",    .print = printInstAndIndex,  .tryEval = tryLoadClosureEval },
      [I_SWAP]             = { .name = "I_SWAP",            .print = printInst,          .tryEval = trySwapEval },

      [I_CONS]             = { .name = "I_CONS",            .print = printInst,          .tryEval = tryConsEval },
      [I_FIRST]            = { .name = "I_FIRST",           .print = printInst,          .tryEval = tryFirstEval},
      [I_REST]             = { .name = "I_REST",            .print = printInst,          .tryEval = tryRestEval },
      [I_SET_MACRO]        = { .name = "I_SET_MACRO",       .print = printInst,          .tryEval = trySetMacroEval},
      [I_GET_MACRO]        = { .name = "I_GET_MACRO",       .print = printInst,          .tryEval = tryGetMacroEval},


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

/*
 * Loading and evaluating code within the VM
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

RetVal tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value **ptr, UnresolvedFnRefs *unresolved, Error *error);

RetVal tryFnHydrate(VM *vm, FnConstant *fnConst, Value *value, Error *error) {
  RetVal ret;

  // always clean up
  UnresolvedFnRefs unresolved;

  // cleanup on failure
  Fn fn;

  unresolvedFnRefsInitContents(&unresolved);

  fn.hasName = fnConst->hasName;
  textInitContents(&fn.name);
  if (fn.hasName) {
    throws(tryTextCopy(&fnConst->name, &fn.name, error));
  }

  fn.numArgs = fnConst->numArgs;
  fn.usesVarArgs = fnConst->usesVarArgs;
  fn.numConstants = fnConst->numConstants;
  fn.numCaptures = fnConst->numCaptures;

  tryMalloc(fn.constants, sizeof(Value) * fn.numConstants, "Value array");
  throws(tryHydrateConstants(vm, fn.numConstants, fnConst->constants, &fn.constants, &unresolved, error));

  throws(tryCodeDeepCopy(&fnConst->code, &fn.code, error));

  throws(tryAllocateFn(&vm->gc, fn, value, error));

  for (uint16_t i=0; i<unresolved.usedSpace; i++) {
    UnresolvedFnRef ref = unresolved.references[i];

    if (ref.fnRef.fnId == fnConst->fnId) {
      // resolve the reference
      fn.constants[ref.constantIndex] = *value;
    }
    else {
      throwRuntimeError(error, "cannot hydrate a reference to a function other than the current one: %llu", ref.fnRef.fnId);
    }
  }

  unresolvedFnRefsFreeContents(&unresolved);

  return R_SUCCESS;

  failure:
    _fnFreeContents(&fn);
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

RetVal tryHydrateConstants(VM *vm, uint16_t numConstants, Constant *constants, Value **ptr, UnresolvedFnRefs *unresolved, Error *error) {
  RetVal ret;

  // clean up on failure
  Value *values = NULL;

  tryMalloc(values, sizeof(Value) * numConstants, "Value array");

  for (uint16_t i=0; i<numConstants; i++) {

    Constant c = constants[i];
    Value v;

    throws(tryHydrateConstant(vm, values, c, &v, i, unresolved, error));

    values[i] = v;
  }

  *ptr = values;
  return R_SUCCESS;

  failure:
    if (values != NULL) {
      free(values);
    }
    return ret;
}

RetVal tryFrameEval(VM *vm, Frame *frame, Error *error) {
  RetVal ret;

  uint8_t inst;
  TryEval tryEval;

  while (!frame->resultAvailable) {

    inst = frame->code.code[frame->pc];
    tryEval = vm->instTable.instructions[inst].tryEval;

    if (tryEval == NULL) {
      throwRuntimeError(error, "instruction unimplemented: %s (%u)", getInstName(&vm->instTable, inst), inst);
    }

    throws(tryEval(vm, frame, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

void topLevelFrameInit(TopLevelFrame *frame) {
  frame->numConstants = 0;
  frame->constants = NULL;
  codeInitContents(&frame->code);
  frame->numLocals = 0;
  frame->locals = NULL;
  opStackInitContents(&frame->opStack);
}

void topLevelFrameFreeContents(TopLevelFrame *topLevel);

RetVal tryTopLevelFrameLoad(VM *vm, TopLevelFrame *topLevel, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  // hydrate all the immediate constant values referenced by the code

  UnresolvedFnRefs unresolved;
  unresolvedFnRefsInitContents(&unresolved);

  topLevel->numConstants = codeUnit->numConstants;
  tryMalloc(topLevel->constants, sizeof(Value) * topLevel->numConstants, "Value array");
  throws(tryHydrateConstants(vm, codeUnit->numConstants, codeUnit->constants, &topLevel->constants, &unresolved, error));

  if (unresolved.usedSpace > 0) {
    throwRuntimeError(error, "no unresolved function references should be present in the top level");
  }

  unresolvedFnRefsFreeContents(&unresolved);

  // deep-copy the code

  throws(tryCodeDeepCopy(&codeUnit->code, &topLevel->code, error));

  // allocate frame space

  topLevel->numLocals = codeUnit->code.numLocals;
  tryMalloc(topLevel->locals, sizeof(Value) * topLevel->numLocals, "Value array for locals");
  throws(tryOpStackInitContents(&topLevel->opStack, codeUnit->code.maxOperandStackSize, error));

  unresolvedFnRefsFreeContents(&unresolved);

  return R_SUCCESS;

  failure:
    topLevelFrameFreeContents(topLevel);
  return ret;
}

void topLevelFrameFreeContents(TopLevelFrame *topLevel) {
  if (topLevel != NULL) {

    topLevel->numConstants = 0;
    if (topLevel->constants != NULL) {
      free(topLevel->constants);
      topLevel->constants = NULL;
    }

    codeFreeContents(&topLevel->code);

    topLevel->numLocals = 0;
    if (topLevel->locals != NULL) {
      free(topLevel->locals);
      topLevel->locals = NULL;
    }

    opStackFreeContents(&topLevel->opStack);
  }
}

RetVal tryVMEval(VM *vm, CodeUnit *codeUnit, Value *result, Error *error) {

  RetVal ret;

  // clean me up on exit
  TopLevelFrame topLevel;
  topLevelFrameInit(&topLevel);

  throws(tryTopLevelFrameLoad(vm, &topLevel, codeUnit, error));

  Frame frame;
  frameInitContents(&frame);
  frame.numConstants = topLevel.numConstants;
  frame.constants = topLevel.constants;
  frame.code = topLevel.code;
  frame.numLocals = topLevel.numLocals;
  frame.locals = topLevel.locals;
  frame.opStack = &topLevel.opStack;

  throws(tryFrameEval(vm, &frame, error));

  topLevelFrameFreeContents(&topLevel);

  *result = frame.result;
  return R_SUCCESS;

  failure:
    topLevelFrameFreeContents(&topLevel);
    return ret;
}

RetVal tryVMPrnRet(VM *vm, Value result, Expr *expr, Error *error) {
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
      throws(tryVMPrnRet(vm, cons.value, elem, error));
      throws(tryListAppend(&expr->list, elem, error));

      while (cons.next.type != VT_NIL) {
        throws(tryDerefCons(&vm->gc, cons.next, &cons, error));
        tryMalloc(elem, sizeof(Expr), "Expr");
        throws(tryVMPrnRet(vm, cons.value, elem, error));
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

RetVal tryVMEvalRet(VM *vm, CodeUnit *codeUnit, Expr *result, Error *error) {

  RetVal ret;

  Value value;
  throws(tryVMEval(vm, codeUnit, &value, error));
  throws(tryVMPrnRet(vm, value, result, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal _tryVMPrnStr(VM_t vm, Value result, StringBuffer_t b, Error *error) {
  RetVal ret;

  switch (result.type) {
    case VT_NIL:
      throws(tryStringBufferAppendStr(b, L"nil", error));
      break;
    case VT_UINT: {
      wchar_t text[256];
      swprintf(text, sizeof(text), L"%llu", result.value);
      throws(tryStringBufferAppendStr(b, text, error));
      break;
    }
    case VT_BOOL:
      if (result.value == 0) {
        throws(tryStringBufferAppendStr(b, L"false", error));
      }
      else {
        throws(tryStringBufferAppendStr(b, L"true", error));
      }
      break;
    case VT_FN:
      throws(tryStringBufferAppendStr(b, L"<function>", error));
      break;
    case VT_CLOSURE:
      throws(tryStringBufferAppendStr(b, L"<closure>", error));
      break;
    case VT_STR: {
      String str;
      throws(tryDerefString(&vm->gc, result, &str, error));
      throws(tryStringBufferAppendChar(b, L'"', error));
      throws(tryStringBufferAppendStr(b, str.value, error));
      throws(tryStringBufferAppendChar(b, L'"', error));
      break;
    }
    case VT_SYMBOL: {
      Symbol sym;
      throws(tryDerefSymbol(&vm->gc, result, &sym, error));
      throws(tryStringBufferAppendStr(b, sym.value, error));
      break;
    }
    case VT_KEYWORD: {
      Keyword kw;
      throws(tryDerefKeyword(&vm->gc, result, &kw, error));
      throws(tryStringBufferAppendChar(b, L':', error));
      throws(tryStringBufferAppendStr(b, kw.value, error));
      break;
    }
    case VT_LIST: {
      Cons cons;
      throws(tryDerefCons(&vm->gc, result, &cons, error));
      throws(tryStringBufferAppendChar(b, L'(', error));
      throws(_tryVMPrnStr(vm, cons.value, b, error));

      while (cons.next.type != VT_NIL) {
        throws(tryDerefCons(&vm->gc, cons.next, &cons, error));
        throws(tryStringBufferAppendChar(b, L' ', error));
        throws(_tryVMPrnStr(vm, cons.value, b, error));
      }

      throws(tryStringBufferAppendChar(b, L')', error));
      break;
    }
    default:
      throwRuntimeError(error, "unsuported value type: %u", result.type);
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVMPrnStr(VM_t vm, Value result, wchar_t **ptr, Error *error) {
  RetVal ret;

  // clean up on exit always
  StringBuffer_t b = NULL;

  throws(tryStringBufferMake(&b, error));
  throws(_tryVMPrnStr(vm, result, b, error));

  wchar_t *output;
  throws(tryCopyText(stringBufferText(b), &output, stringBufferLength(b), error));
  stringBufferFree(b);

  *ptr = output;
  return R_SUCCESS;

  failure:
    stringBufferFree(b);
    return ret;
}

RetVal tryVMPrn(VM_t vm, Value result, Error *error) {
  RetVal ret;

  wchar_t *str = NULL;
  throws(tryVMPrnStr(vm, result, &str, error));
  printf("%ls", str);
  free(str);

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryVMInitContents(VM *vm , Error *error) {
  RetVal ret;

  vm->instTable = instTableCreate();
  GCInit(&vm->gc);
  throws(tryNamespacesInitContents(&vm->namespaces, error));

  ret = R_SUCCESS;
  return ret;

  failure:
  return ret;
}

void vmFreeContents(VM *vm) {
  if (vm != NULL) {
    GCFreeContents(&vm->gc);
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
 * TODO: the value types need a common protocol struct to hold function impls for all the operations
 * that are common between them so we don't end up with switch statements everywhere
 *
 * TODO: the instruction types need the same treatment
 */

/*
 * Here begin scratch thoughts that are probably mostly invalid by the time anyone reads them.
 */

// hold a table of Value references to functions and constants defined internally within this function
// this way this function can be hydrated from a CodeUnit once, and have all its inner functions
// hydrated once as well.

// functions should not have 'inner' functions. The bytecode format should require that all functions definittions
// be hoisted to the top level. code that references functions expliclitly by value, and not indirectly, can use
// the mechanism below:

// going from a CodeUnit to a hydrated representation of a function should not result in a code rewrite
// you can avoid a rewrite by having the bytecode say "load the value of this function reference", where the
// reference is mapped in the header of the Fn to an actual object reference to the function in question. This can
// be populated at the time that the Fn is loaded as a value for the first time.

// a big part of my confusion has been conflating function loading with function invocation/execution

// It must be a property of functions that they retain references to the values they explicitly reference
// beyond a given function call, otherwise all these values have to be re-hydrated from the CodeUnit every time...

//};

// there are three concepts I'd like to separate:
// 1. the CodeUnit that is generated for a function
//    - this includes the actual instructions
//    - this also includes source metadata and constants
//    - this should live on the heap, and be gc-able
// 2. the actual value that can be used to invoke that function
//    - this should really be a singleton
//    - this should live on the heap, and be gc-able
//    - this should hold an object reference to its CodeUnit to prevent it from being gc-ed too soon
// 3. the way this value is discovered by code that wants to invoke it
//    - a literal function reference, returned as a value from another function call
//    - a local
//    - a var
// and yet...
// it would be much easier to duplicate the constants into the functions themselves where needed

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
//F_FN_CALL -> I_INVOKE_DYN

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
