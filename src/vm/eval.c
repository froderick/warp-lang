#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <inttypes.h>
#include <errno.h>

#include "../errors.h"
#include "exception.h"
#include "builtin.h"
#include "frame.h"
#include "heap.h"

#include "internal.h"

/*
 * Loading Constants as Values
 */

typedef struct Invocable {
  Value ref;
  Fn *fn;
  Closure *closure;
} Invocable;

static Value _hydrateConstant(VM *vm, Constant c);

static Value _fnHydrate(VM *vm, FnConstant *fnConst) {

  Fn *fn = NULL;
  {
    uint64_t nameSize = 0;
    if (fnConst->hasName) {
      nameSize = (wcslen(fnConst->name) + 1) * sizeof(wchar_t);
    }
    uint64_t constantsSize = fnConst->numConstants * sizeof(Value);
    uint64_t codeSize = fnConst->code.codeLength * sizeof(uint8_t);
    uint64_t sourceFileNameSize = 0;
    if (fnConst->code.hasSourceTable) {
      sourceFileNameSize = (wcslen(fnConst->code.sourceTable.fileName) + 1) * sizeof(wchar_t);
    }
    uint64_t lineNumbersSize = fnConst->code.sourceTable.numLineNumbers * sizeof(LineNumber);

    uint64_t fnSize = padAllocSize(
        sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize + lineNumbersSize);

    fn = alloc(vm, fnSize);

    fnInitContents(fn);

    fn->header = makeObjectHeader(W_FN_TYPE, fnSize);

    fn->nameOffset = sizeof(Fn);
    fn->constantsOffset = sizeof(Fn) + nameSize;
    fn->codeOffset = sizeof(Fn) + nameSize + constantsSize;
    fn->sourceFileNameOffset = sizeof(Fn) + nameSize + constantsSize + codeSize;
    fn->lineNumbersOffset = sizeof(Fn) + nameSize + constantsSize + codeSize + sourceFileNameSize;

    fn->hasName = fnConst->hasName;
    if (fn->hasName) {
      fn->nameLength = wcslen(fnConst->name);
      size_t copySize = fn->nameLength * sizeof(wchar_t);

      memcpy(fnName(fn), fnConst->name, copySize);
      fnName(fn)[fn->nameLength] = L'\0';
    }

    fn->numArgs = fnConst->numArgs;
    fn->usesVarArgs = fnConst->usesVarArgs;

    fn->numConstants = fnConst->numConstants;

    fn->numLocals = fnConst->code.numLocals;
    fn->maxOperandStackSize = fnConst->code.maxOperandStackSize;

    {
      fn->codeLength = fnConst->code.codeLength;
      memcpy(fnCode(fn), fnConst->code.code, codeSize);
    }

    fn->hasSourceTable = fnConst->code.hasSourceTable;
    if (fn->hasSourceTable) {

      fn->sourceFileNameLength = wcslen(fnConst->code.sourceTable.fileName);
      size_t copySize = fn->sourceFileNameLength * sizeof(wchar_t);

      memcpy(fnSourceFileName(fn), fnConst->code.sourceTable.fileName, copySize);
      fnSourceFileName(fn)[fn->sourceFileNameLength] = L'\0';

      fn->numLineNumbers = fnConst->code.sourceTable.numLineNumbers;
      memcpy(fnLineNumbers(fn), fnConst->code.sourceTable.lineNumbers, lineNumbersSize);
    }
  }

  for (uint16_t i=0; i<fn->numConstants; i++) {
    fnConstants(fn)[i] = W_NIL_VALUE;
  }

  pushFrameRoot(vm, (Value*)&fn);
  for (uint16_t i=0; i<fn->numConstants; i++) {
    Value hydrated = _hydrateConstant(vm, fnConst->constants[i]);
    fnConstants(fn)[i] = hydrated;
  }
  popFrameRoot(vm);

  return (Value)fn;
}

static Value _symbolHydrate(VM *vm, SymbolConstant symConst) {

  Value protectedName = makeStringValue(vm, symConst.value, symConst.length);
  pushFrameRoot(vm, &protectedName);

  Value value = symbolIntern(vm, &protectedName);

  popFrameRoot(vm);
  return value;
}

static Value _keywordHydrate(VM *vm, KeywordConstant kwConst) {
  Value protectedName = makeStringValue(vm, kwConst.value, kwConst.length);
  pushFrameRoot(vm, &protectedName);

  Value value = keywordIntern(vm, &protectedName);

  popFrameRoot(vm);
  return value;
}

// TODO: I had another thought, can we get rid of the nested graph of constants and flatten it entirely?

static Value _hydrateConstant(VM *vm, Constant c) {
  Value v;
  switch (c.type) {
    case CT_BOOL:
      v = wrapBool(c.boolean);
      break;
    case CT_INT:
      v = wrapUint(c.integer);
      break;
    case CT_CHAR:
      v = wrapChar(c.chr);
      break;
    case CT_NIL:
      v = W_NIL_VALUE;
      break;
    case CT_FN:
      v = _fnHydrate(vm, &c.function);
      break;
    case CT_STR:
      v = makeStringValue(vm, c.string.value, c.string.length);
      break;
    case CT_SYMBOL:
      v = _symbolHydrate(vm, c.symbol);
      break;
    case CT_KEYWORD:
      v = _keywordHydrate(vm, c.keyword);
      break;
    case CT_NONE:
    default:
      explode("invalid constant: %u", c.type);
  }
  return v;
}

static void _invocableInitContents(Invocable *i) {
  i->ref = W_NIL_VALUE;    // the reference to the initially invoked value (could be closure or fn)
  i->fn = NULL;      // always points to the actual fn
  i->closure = NULL; // points to the closure, if there is one
}

static int _makeInvocable(VM *vm, Value pop, Invocable *invocable) {

  _invocableInitContents(invocable);
  invocable->ref = pop;

  ValueType fnRefType = valueType(invocable->ref);
  switch (fnRefType) {
    case VT_FN: {
      invocable->fn = deref(vm, invocable->ref);
      invocable->closure = NULL;
      break;
    }
    case VT_CLOSURE: {
      invocable->closure = deref(vm, invocable->ref);
      invocable->fn = deref(vm, invocable->closure->fn);
      break;
    }
    default:
      // fail: not all values are invocable
      raise(vm, "cannot invoke this value type as a function: %s",
          getValueTypeName(vm, fnRefType));
      return R_ERROR;
  }

  return R_SUCCESS;
}

static void _protectInvocable(VM *vm, Invocable *invocable) {
  pushFrameRoot(vm, &invocable->ref);
  pushFrameRoot(vm, (Value*)&invocable->fn);
  if (invocable->closure != NULL) {
    pushFrameRoot(vm, (Value*) &invocable->closure);
  }
}

static void _unprotectInvocable(VM *vm, Invocable *invocable) {
  popFrameRoot(vm);
  popFrameRoot(vm);
  if (invocable->closure != NULL) {
    popFrameRoot(vm);
  }
}

static void _populateArgs(Frame_t from, Frame_t to, uint16_t numArgs) {
  for (uint16_t i = 0; i < numArgs; i++) {
    Value arg = popOperand(from);

    uint16_t idx = numArgs - (1 + i);
    setLocal(to, idx, arg);
  }
}

static void _preprocessArguments(VM *vm, Frame_t parent, uint16_t numArgs, bool usesVarArgs, uint64_t numArgsSupplied) {
  if (usesVarArgs) {

    uint16_t numVarArgs;
    if (numArgsSupplied > numArgs) {
      numVarArgs = (numArgsSupplied - numArgs) + 1;
    }
    else if (numArgsSupplied == numArgs) {
      numVarArgs = 1;
    }
    else if (numArgsSupplied == numArgs - 1) {
      numVarArgs = 0;
    }
    else {
      explode("insuficient arguments supplied");
    }

    Value seq = W_NIL_VALUE;
    pushFrameRoot(vm, &seq);

    // read the extra args into that sequence, push it back on the stack
    for (uint16_t i = 0; i < numVarArgs; i++) {
      Cons *cons = makeCons(vm);
      cons->value = popOperand(parent);
      cons->next = seq;
      seq = (Value)cons;
    }

    popFrameRoot(vm);
    pushOperand(parent, seq);
  }
}

static void _invokePopulateLocals(VM *vm, Frame_t parent, Frame_t child, Invocable *invocable, uint16_t numArgsSupplied) {

  _protectInvocable(vm, invocable);

  _preprocessArguments(vm, parent, invocable->fn->numArgs, invocable->fn->usesVarArgs, numArgsSupplied);

  _populateArgs(parent, child, invocable->fn->numArgs);

  uint16_t numCaptures = 0;

  if (invocable->closure != NULL) {
    numCaptures = invocable->closure->numCaptures;
    uint16_t nextLocalIdx = invocable->fn->numArgs;
    for (uint16_t i=0; i<invocable->closure->numCaptures; i++) {
      setLocal(child, nextLocalIdx, closureCaptures(invocable->closure)[i]);
      nextLocalIdx = nextLocalIdx + 1;
    }
  }

  if (invocable->fn->hasName) {
    uint16_t fnLocalIndex = invocable->fn->numArgs + numCaptures;
    setLocal(child, fnLocalIndex, invocable->ref);
  }

  _unprotectInvocable(vm, invocable);
}

static bool _handleRaise(VM *vm) {
  Value exception = vm->exception;

  if (vm->current != NULL) { // a stack is available, look for a handler

    Frame_t f = vm->current;
    FrameHandler_t h = NULL;

    while (true) {

      FrameHandler_t current = currentHandler(f);
      if (current != NULL) { // a handler is already running, start with its immediate parent if it has one

        // discard handler frame
        f = getParent(f);

        // pop current handler from handling frame, since we already used it
        popSpecificFrameHandler(f);

        // use the parent handler within the handling frame, if any
        h = frameHandlers(f);
      }
      else { // no handler is running, look for the nearest one in the current frame
        h = frameHandlers(f);
      }

      if (h != NULL) {
        break;
      }
      else {
        if (hasParent(f)) {
          Frame_t frameToPop = f;
          f = getParent(f);
          popSpecificFrame(vm, frameToPop);
        } else {
          break;
        }
      }
    }

    if (h != NULL) { // found a handler

      {
        pushOperand(f, exception);

        Invocable invocable;
        _makeInvocable(vm, *frameHandlerValue(h), &invocable);

        Frame_t handlerFrame = pushFrame(vm, (Value) invocable.fn);
        _invokePopulateLocals(vm, f, handlerFrame, &invocable, 1);

        setCurrentHandler(handlerFrame, h);
      }

      uint16_t jumpAddr = frameHandlerJumpAddr(h);
      setPc(f, jumpAddr);

      clearException(vm);
      return true;
    }
  }

  // no handler, bomb out
  return false;
}

/*
 * Instruction Definitions
 */

// (8), typeIndex (16) | (-> value)
static int _loadConstEval(VM *vm, Frame_t frame) {
  uint16_t constantIndex = readIndex(frame);
  Value constant = getConst(frame, constantIndex);
  pushOperand(frame, constant);
  return R_SUCCESS;
}

// (8), typeIndex (16) | (-> value)
static int _loadLocalEval(VM *vm, Frame_t frame) {
  uint16_t localIndex = readIndex(frame);
  Value v = getLocal(frame, localIndex);
  pushOperand(frame, v);
  return R_SUCCESS;
}

// (8), typeIndex  (16) | (objectref ->)
static int _storeLocalEval(VM *vm, Frame_t frame) {
  uint16_t localIndex = readIndex(frame);
  Value v = popOperand(frame);
  setLocal(frame, localIndex, v);
  return R_SUCCESS;
}

static int _validateArguments(VM *vm, wchar_t *name, uint16_t numArgs, bool usesVarArgs, uint64_t numArgsSupplied) {

  if (!usesVarArgs) {
    if (numArgsSupplied != numArgs) {
      raise(vm, "%ls: required arguments not supplied, expected %u but got %" PRIu64, name, numArgs,
            numArgsSupplied);
      return R_ERROR;
    }
  }
  else {
    uint16_t numVarArgs;
    if (numArgsSupplied > numArgs) {
      numVarArgs = (numArgsSupplied - numArgs) + 1;
    }
    else if (numArgsSupplied == numArgs) {
      numVarArgs = 1;
    }
    else if (numArgsSupplied == numArgs - 1) {
      numVarArgs = 0;
    }
    else {
      raise(vm, "%ls: required arguments not supplied, expected %u or more arguments but got %" PRIu64,
            name, numArgs - 1, numArgsSupplied);
      return R_ERROR;
    }
  }


  return R_SUCCESS;
}

static int _invokeCFn(VM *vm, Frame_t frame, Value cFn, uint16_t numArgsSupplied) {
  CFn *protectedFn = deref(vm, cFn);
  pushFrameRoot(vm, (Value*)&protectedFn);

  int error = _validateArguments(vm, cFnName(protectedFn), protectedFn->numArgs, protectedFn->usesVarArgs, numArgsSupplied);
  if (error) {
    goto cleanup;
  }

  _preprocessArguments(vm, frame, protectedFn->numArgs, protectedFn->usesVarArgs, numArgsSupplied);

  error = ((CFnInvoke)protectedFn->ptr)(vm, frame);
  if (error) {
    goto cleanup;
  }

  cleanup:
    popFrameRoot(vm);
    if (error) {
      return error;
    }
    else {
      return R_SUCCESS;
    }
}

// (8)              | (objectref, args... -> ...)
static int _invokeDynEval(VM *vm, Frame_t frame) {
  uint16_t numArgsSupplied = readIndex(frame);
  Value pop = popOperand(frame);
  switch (valueType(pop)) {
    case VT_CFN: {
      int error = _invokeCFn(vm, frame, pop, numArgsSupplied);
      if (error) {
        return error;
      }
      break;
    }
    case VT_KEYWORD: {
      Value key = pop;

      int error = _validateArguments(vm, L"keyword-get", 1, false, numArgsSupplied);
      if (error) {
        return error;
      }

      _preprocessArguments(vm, frame, 1, false, numArgsSupplied);
      Value coll = popOperand(frame);
      Map *m = deref(vm, coll);
      Value result = mapLookup(vm, m, key);
      pushOperand(frame, result);
      break;
    }
    default: {

      Invocable invocable;
      Frame_t parent;

      int error = _makeInvocable(vm, pop, &invocable);
      if (error) {
        return error;
      }

      error = _validateArguments(vm, fnName(invocable.fn), invocable.fn->numArgs, invocable.fn->usesVarArgs, numArgsSupplied);
      if (error) {
        return error;
      }

      frame = pushFrame(vm, (Value) invocable.fn);
      parent = getParent(frame);
      _invokePopulateLocals(vm, parent, frame, &invocable, numArgsSupplied);
    }
  }

  return R_SUCCESS;
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
static int _invokeDynTailEval(VM *vm, Frame_t frame) {
  uint16_t numArgsSupplied = readIndex(frame);
  Value pop = popOperand(frame);
  switch (valueType(pop)) {
    case VT_CFN: {
      int error = _invokeCFn(vm, frame, pop, numArgsSupplied);
      if (error) {
        return error;
      }
      break;
    }
    case VT_KEYWORD: {
      Value key = pop;

      int error = _validateArguments(vm, L"keyword-get", 1, false, numArgsSupplied);
      if (error) {
        return error;
      }

      _preprocessArguments(vm, frame, 1, false, numArgsSupplied);
      Value coll = popOperand(frame);
      Map *m = deref(vm, coll);
      Value result = mapLookup(vm, m, key);
      pushOperand(frame, result);
      break;
    }
    default: {
      Invocable invocable;

      int error = _makeInvocable(vm, pop, &invocable);
      if (error) {
        return error;
      }

      error = _validateArguments(vm, fnName(invocable.fn), invocable.fn->numArgs, invocable.fn->usesVarArgs, numArgsSupplied);
      if (error) {
        return error;
      }

      replaceFrame(vm, (Value) invocable.fn);
      _invokePopulateLocals(vm, frame, frame, &invocable, numArgsSupplied);
    }
  }

  return R_SUCCESS;
}

// (8)              | (args... -> ...)
static int _invokeDynTailEvalRecurse(VM *vm, Frame_t frame) {
  uint16_t numArgs = readIndex(frame);
  _populateArgs(frame, frame, numArgs);
  setPc(frame, 0);
  return R_SUCCESS;
}

// (8)              | (objectref ->)
static int _retEval(VM *vm, Frame_t frame) {
  Value v = popOperand(frame);
  setResult(frame, v);
  return R_SUCCESS;
}

// (8), offset (16) | (->)
static int _jmpEval(VM *vm, Frame_t frame) {
  uint16_t newPc = readIndex(frame);
  setPc(frame, newPc);
  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
static int _jmpIfEval(VM *vm, Frame_t frame) {
  Value test = popOperand(frame);
  bool truthy = isTruthy(vm, test);
  uint16_t newPc = readIndex(frame);
  if (truthy) {
    setPc(frame, newPc);
  }
  return R_SUCCESS;
}

// (8), offset (16) | (value ->)
static int _jmpIfNotEval(VM *vm, Frame_t frame) {
  Value test = popOperand(frame);
  bool truthy = isTruthy(vm, test);
  uint16_t newPc = readIndex(frame);
  if (!truthy) {
    setPc(frame, newPc);
  }
  return R_SUCCESS;
}

// (8), offset (16)  | (value ->)
static int _defVarEval(VM *vm, Frame_t frame) {

  Value value = popOperand(frame);
  uint16_t constantIndex = readIndex(frame);
  Value varName = getConst(frame, constantIndex);

  Symbol *symbol = deref(vm, varName);

  String *name = deref(vm, symbol->name);
  printf("defining %ls\n", stringValue(name));

  symbol->valueDefined = true;
  symbol->topLevelValue = value;

  pushOperand(frame, W_NIL_VALUE);
  return R_SUCCESS;
}

// (8), offset 16  | (-> value)
static int _loadVarEval(VM *vm, Frame_t frame) {

  uint16_t constantIndex = readIndex(frame);
  Value value = getConst(frame, constantIndex);
  ValueType varNameType = valueType(value);

  if (varNameType != VT_SYMBOL) {
    explode("expected a symbol: %s", getValueTypeName(vm, varNameType));
  }

  Symbol *symbol = deref(vm, value);

  if (!symbol->valueDefined) {

    for (uint64_t i=0; i<vm->symbolTable.numAllocatedEntries; i++) {
      TableEntry *e = &vm->symbolTable.entries[i];
      if (e->used) {
        Symbol *s = deref(vm, e->value);
        String *name = deref(vm, s->name);
        printf("symbol-table: %ls (defined=%u)\n", stringValue(name), s->valueDefined);
      }
    }

    String *name = deref(vm, symbol->name);
    raise(vm, "no value defined for : '%ls'", stringValue(name));
    return R_ERROR;
  }

  pushOperand(frame, symbol->topLevelValue);
  return R_SUCCESS;
}

static void _closureInitContents(Closure *cl) {
  cl->header = 0;
  cl->fn = W_NIL_VALUE;
  cl->numCaptures = 0;
  cl->capturesOffset = 0;
}

// (8), offset (16) | (captures... -> value)
static int _loadClosureEval(VM *vm, Frame_t frame) {
  Fn *protectedFn;
  {
    uint16_t constantIndex = readIndex(frame);
    Value fnValue = getConst(frame, constantIndex);

    ValueType fnValueType = valueType(fnValue);
    if (fnValueType != VT_FN) {
      raise(vm, "cannot create a closure from this value type: %s", getValueTypeName(vm, fnValueType));
      return R_ERROR;
    }

    protectedFn = deref(vm, fnValue);
  }
  pushFrameRoot(vm, (Value*)&protectedFn);

  uint16_t numCaptures = readIndex(frame);

  uint64_t capturesSize = numCaptures * sizeof(Value);
  uint64_t clSize = padAllocSize(sizeof(Closure) + capturesSize);
  Closure *closure = alloc(vm, clSize);

  _closureInitContents(closure);
  closure->header = makeObjectHeader(W_CLOSURE_TYPE, clSize);
  closure->fn = (Value)protectedFn;
  closure->numCaptures = numCaptures;

  closure->capturesOffset = sizeof(Closure);

  // pop captures in reverse order, same as arguments
  for (uint16_t i=0; i<closure->numCaptures; i++) {
    Value capture = popOperand(frame);
    uint16_t idx = closure->numCaptures - (1 + i);
    closureCaptures(closure)[idx] = capture;
  }

  popFrameRoot(vm);
  pushOperand(frame, (Value)closure);
  return R_SUCCESS;
}

// (8)        | (a, b -> b, a)
static int _swapEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  Value b = popOperand(frame);
  pushOperand(frame, a);
  pushOperand(frame, b);
  return R_SUCCESS;
}

// (8)        | (a, b -> b, a)
static int _dropEval(VM *vm, Frame_t frame) {
  Value a = popOperand(frame);
  return R_SUCCESS;
}

// (8)        | (jumpAddr, handler ->)
static int _setHandlerEval(VM *vm, Frame_t frame) {
  uint16_t jumpIndex = readIndex(frame);

  Value handler = popOperand(frame);
  ValueType handlerType = valueType(handler);
  if (handlerType != VT_FN) {
    raise(vm, "handlers must be of type fn: %s", getValueTypeName(vm, handlerType));
    return R_ERROR;
  }

  pushFrameHandler(vm, handler, jumpIndex);
  return R_SUCCESS;
}

// (8)        | (->)
static int _clearHandlerEval(VM *vm, Frame_t frame) {
  popFrameHandler(vm);
  return R_SUCCESS;
}


static void _printInst(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\n", *i, name);
}

static void _printInstAndIndex(int *i, const char* name, uint8_t *code) {
  printf("%i:\t%s\t%u\n", *i, name, code[*i + 1] << 8 | code[*i + 2]);
  *i = *i + 2;
}

static void _printInstAndIndex2x(int *i, const char* name, uint8_t *code) {
  uint16_t index1 = code[*i + 1] << 8 | code[*i + 2];
  uint16_t index2 = code[*i + 3] << 8 | code[*i + 4];
  printf("%i:\t%s\t%u, %u\n", *i, name, index1, index2);
  *i = *i + 4;
}

static void _printUnknown(int *i, const char* name, uint8_t *code) {
  printf("%i:\t<UNKNOWN>/%u\n", *i, code[*i]);
}

InstTable instTableCreate() {
  InstTable table;

  // init table with blanks
  uint16_t instructionsAllocated = sizeof(table.instructions) / sizeof(table.instructions[0]);
  for (int i=0; i<instructionsAllocated; i++) {
    table.instructions[i].name = NULL;
    table.instructions[i].print = NULL;
    table.instructions[i].eval = NULL;
  }

  // init with known instructions
  Inst instructions[]      = {
      [I_LOAD_CONST]       = { .name = "I_LOAD_CONST",      .print = _printInstAndIndex,   .eval = _loadConstEval },
      [I_LOAD_LOCAL]       = { .name = "I_LOAD_LOCAL",      .print = _printInstAndIndex,   .eval = _loadLocalEval },
      [I_STORE_LOCAL]      = { .name = "I_STORE_LOCAL",     .print = _printInstAndIndex,   .eval = _storeLocalEval },
      [I_INVOKE_DYN]       = { .name = "I_INVOKE_DYN",      .print = _printInstAndIndex,   .eval = _invokeDynEval },
      [I_INVOKE_DYN_TAIL]  = { .name = "I_INVOKE_DYN_TAIL", .print = _printInstAndIndex,   .eval = _invokeDynTailEval },
      [I_INVOKE_DYN_TAIL_RECURSE]  = { .name = "I_INVOKE_DYN_TAIL_RECURSE", .print = _printInstAndIndex,   .eval = _invokeDynTailEvalRecurse},
      [I_RET]              = { .name = "I_RET",             .print = _printInst,           .eval = _retEval },
      [I_CMP]              = { .name = "I_CMP",             .print = _printInst,           .eval = cmpEval },
      [I_JMP]              = { .name = "I_JMP",             .print = _printInstAndIndex,   .eval = _jmpEval },
      [I_JMP_IF]           = { .name = "I_JMP_IF",          .print = _printInstAndIndex,   .eval = _jmpIfEval },
      [I_JMP_IF_NOT]       = { .name = "I_JMP_IF_NOT",      .print = _printInstAndIndex,   .eval = _jmpIfNotEval },
      [I_ADD]              = { .name = "I_ADD",             .print = _printInst,           .eval = addEval },
      [I_SUB]              = { .name = "I_SUB",             .print = _printInst,           .eval = subEval },
      [I_DEF_VAR]          = { .name = "I_DEF_VAR",         .print = _printInstAndIndex,   .eval = _defVarEval },
      [I_LOAD_VAR]         = { .name = "I_LOAD_VAR",        .print = _printInstAndIndex,   .eval = _loadVarEval },
      [I_LOAD_CLOSURE]     = { .name = "I_LOAD_CLOSURE",    .print = _printInstAndIndex2x, .eval = _loadClosureEval },
      [I_SWAP]             = { .name = "I_SWAP",            .print = _printInst,           .eval = _swapEval },
      [I_PUSH_HANDLER]     = { .name = "I_PUSH_HANDLER",     .print = _printInstAndIndex,   .eval = _setHandlerEval },
      [I_POP_HANDLER]      = { .name = "I_POP_HANDLER",   .print = _printInst,           .eval = _clearHandlerEval },
      [I_CONS]             = { .name = "I_CONS",            .print = _printInst,           .eval = consEval },
      [I_DROP]             = { .name = "I_DROP",            .print = _printInst,           .eval = _dropEval},

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

static const char* _getInstName(InstTable *instTable, uint8_t inst) {
  return instTable->instructions[inst].name;
}

static void _frameEval(VM *vm) {
  uint8_t inst;
  Eval eval;

  while (true) {

    if (hasResult(vm->current)) {
      if (!hasParent(vm->current)) {
        break;
      }
      else {
        Value result = getResult(vm->current);
        Frame_t parent = getParent(vm->current);
        pushOperand(parent, result);
        popFrame(vm);
      }
    }

    inst = readInstruction(vm->current);
    eval = vm->instTable.instructions[inst].eval;
    if (eval == NULL) {
      explode("instruction unimplemented: %s (%u)", _getInstName(&vm->instTable, inst), inst);
    }

    int raised = eval(vm, vm->current);
    if (raised) {
      bool handled = _handleRaise(vm);
      if (handled) { // exception thrown and handled, keep evaluating
        continue;
      }
      else {
        break;
      }
    }
  }
}

VMEvalResult vmEval(VM *vm, CodeUnit *codeUnit) {

  // set up
  {
    FnConstant c;
    constantFnInitContents(&c);
    c.numConstants = codeUnit->numConstants;
    c.constants = codeUnit->constants;
    c.code = codeUnit->code;

    Value fnRef = _fnHydrate(vm, &c);
    pushFrame(vm, fnRef);
  }

  VMEvalResult result;

  _frameEval(vm);
  if (!hasException(vm)) {
    result.type = RT_RESULT;
    result.value = vm->current->result;
  }
  else {
    result.type = RT_EXCEPTION;
    result.value = getException(vm);
    clearException(vm);
  }

  // clean up
  popFrame(vm);

  if (vm->current != NULL) {
    explode("dangling frames found!\n");
  }

  return result;
}
