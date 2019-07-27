#include "compiler.h"
#include "string.h"

typedef struct Constants {
  uint16_t numAllocated;
  uint16_t numUsed;
  Constant *constants;
} Constants;

typedef struct Codes {
  uint16_t numAllocated;
  uint16_t numUsed;
  uint8_t *codes;
} Codes;

typedef struct LineNumbers {
  uint16_t numAllocated;
  uint16_t numUsed;
  LineNumber *numbers;
} LineNumbers;

typedef struct Output {
  Pool_t pool;
  Constants *constants;
  Codes *codes;
  uint16_t *slotsTable; // maps binding table indexes to slot indexes for storing locals
  LineNumbers *lineNumbers;
  bool hasFileName;
  Text fileName;
} Output;

void constantsInitContents(Constants *constants) {
  constants->numAllocated = 0;
  constants->numUsed = 0;
  constants->constants = NULL;
}

RetVal tryAppendConstant(Output output, Constant c, Error *error) {
  RetVal ret;

  Constants *constants = output.constants;

  if (constants->constants == NULL) {
    uint16_t len = 16;
    tryPalloc(output.pool, constants->constants, len * sizeof(Constant), "Constant array");
    constants->numAllocated = len;
  }
  else if (constants->numUsed == constants->numAllocated) {
    uint16_t newAllocatedLength = constants->numAllocated * 2;

    Constant *resized = NULL;
    tryPalloc(output.pool, resized, newAllocatedLength * sizeof(Constant), "Constant array");
    memcpy(resized, constants->constants, constants->numUsed * sizeof(Constant));

    constants->numAllocated = newAllocatedLength;
    constants->constants = resized;
  }

  uint16_t index = constants->numUsed;
  constants->constants[index] = c;
  constants->numUsed = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

void codesInitContents(Codes *codes) {
  codes->numAllocated = 0;
  codes->numUsed = 0;
  codes->codes = NULL;
}

RetVal tryCodeAppend(Output output, uint16_t numAdded, uint8_t *added, Error *error) {
  RetVal ret;

  Codes *codes = output.codes;

  if (codes->codes == NULL) {
    uint16_t len = 16;
    tryPalloc(output.pool, codes->codes, len * sizeof(uint8_t), "byte array");
    codes->numAllocated = len;
  }
  else if (codes->numUsed + numAdded > codes->numAllocated) {
    uint16_t newAllocatedLength = (codes->numAllocated + numAdded) * 2;

    uint8_t *replacement = NULL;
    tryPalloc(output.pool, replacement, newAllocatedLength * sizeof(uint8_t), "byte array");
    memcpy(replacement, codes->codes, codes->numUsed * sizeof(uint8_t));

    codes->numAllocated = newAllocatedLength;
    codes->codes = replacement;
  }

  uint16_t baseIndex = codes->numUsed;
  for (uint16_t i=0; i<numAdded; i++) {
    codes->codes[baseIndex + i] = added[i];
  }
  codes->numUsed += numAdded;

  return R_SUCCESS;

  failure:
    return ret;
}

void lineNumbersInitContents(LineNumbers *numbers) {
  numbers->numAllocated = 0;
  numbers->numUsed = 0;
  numbers->numbers = NULL;
}

RetVal tryLineNumbersAppend(Pool_t pool, LineNumbers *numbers, LineNumber number, Error *error) {
  RetVal ret;

  if (numbers->numbers == NULL) {
    uint16_t len = 16;
    tryPalloc(pool, numbers->numbers, len * sizeof(LineNumber), "LineNumber array");
    numbers->numAllocated = len;
  }
  else if (numbers->numUsed == numbers->numAllocated) {
    uint16_t newAllocatedLength = numbers->numAllocated * 2;

    LineNumber *resized = NULL;
    tryPalloc(pool, resized, newAllocatedLength * sizeof(LineNumber), "LineNumber array");
    memcpy(resized, numbers->numbers, numbers->numUsed * sizeof(LineNumber));

    numbers->numAllocated = newAllocatedLength;
    numbers->numbers = resized;
  }

  uint16_t index = numbers->numUsed;
  numbers->numbers [index] = number;
  numbers->numUsed = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

// compiler behavior that emits constants and code

/*
 * If present, appends the source line number from the form to the
 * *next* code index to be appended.
 */
RetVal tryAppendSource(Form *form, Output output, Error *error) {
  RetVal ret;

  if (form->source.isSet) {

    if (form->source.lineNumber == 0) {
      throwCompilerError(error, "line number is zero");
    }

    LineNumber lineNumber;
    lineNumberInitContents(&lineNumber);
    lineNumber.lineNumber = form->source.lineNumber;
    lineNumber.startInstructionIndex = output.codes->numUsed;

    throws(tryLineNumbersAppend(output.pool, output.lineNumbers, lineNumber, error));
  }

  return R_SUCCESS;

  failure:
  return ret;
}

void setIndexAtOffset(Output output, uint16_t index, uint16_t value) {
  output.codes->codes[index] = value >> 8;
  output.codes->codes[index + 1] = value & 0xFF;
}

RetVal tryCompile(Form *form, Output output, Error *error);

RetVal nilConstantGetIndex(Output output, uint16_t *index, Error *error);

RetVal tryCompileIf(Form *form, Output output, Error *error) {
  RetVal ret;

  // emit test
  // emit I_JMP_IF_NOT $ELSE_ADDR
  // emit ifBranch {
  //   - do stuff
  //   - JMP to $END_ADDR
  // }
  // $ELSE_ADDR
  // emit elseBranch {
  //   - do stuff
  // }
  // $END_ADDR

  // emit the test code
  throws(tryCompile(form->iff.test, output, error));

  // emit the testFailedJump code, keep a pointer to the jump address
  uint16_t testFailedJumpAddrOffset;
  {
    uint8_t testFailedJumpCode[] = { I_JMP_IF_NOT, 0, 0 };
    throws(tryCodeAppend(output, sizeof(testFailedJumpCode), testFailedJumpCode, error));
    testFailedJumpAddrOffset = output.codes->numUsed - 2;
  }

  // emit ifBranch form code
  throws(tryCompile(form->iff.ifBranch, output, error));

  // emit the jumpToEnd code to terminate the if branch, keep a pointer to the jump address
  uint16_t jumpToEndAddrOffset;
  {
    uint8_t jumpToEndCode[] = {I_JMP, 0, 0};
    throws(tryCodeAppend(output, sizeof(jumpToEndCode), jumpToEndCode, error));
    jumpToEndAddrOffset = output.codes->numUsed - 2;
  }

  // the testFailedJumpAddr should point to the first address after the ifBranch
  {
    uint16_t nextCodeAddr = output.codes->numUsed;
    setIndexAtOffset(output, testFailedJumpAddrOffset, nextCodeAddr);
  }

  // emit elseBranch form code
  if (form->iff.elseBranch != NULL) {
    throws(tryCompile(form->iff.elseBranch, output, error));
  }
  else {
    uint16_t index;
    throws(nilConstantGetIndex(output, &index, error));
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }

  // the jumpToEndAddr should point to the first address after the elseBranch
  {
    uint16_t nextCodeAddr = output.codes->numUsed;
    setIndexAtOffset(output, jumpToEndAddrOffset, nextCodeAddr);
  }

  return R_SUCCESS;
  failure:
    return ret;
}

// TODO: perhaps we need an initial pass before compilation to identify constants, and references to constants
// so that we can avoid encoding the same constants repeatedly where they are referenced more than once

/*
 * Here is how this code expects the VM to populate the locals when this function is invoked:
 * - if the function defines a self-referential name, store that as a constant in the first slot
 * - then store all the captured bindings as locals next
 * - then store all the arguments as locals next
 * - the remaining locals space is left blank for let-bindings
 *
 * // TODO: the compiler should ensure this order rather than depending on the analyzer's sequence,
 * // since the virtual machine does not have the binding table at its disposal to inspect
 */

RetVal trySlotsTableBuild(Pool_t pool, BindingTable *bindingTable, uint16_t **ptr, Error *error) {
  RetVal ret;

  uint16_t *slotsTable = NULL;

  tryPalloc(pool, slotsTable, sizeof(uint16_t) * bindingTable->usedSpace, "uint16_t array");

  uint16_t slotsCounter = 0;

  // make sure we only got binding types we recognize
  for (uint16_t i=0; i<bindingTable->usedSpace; i++) {
    Binding *b = &bindingTable->bindings[i];

    switch (b->source) {

      case BS_LOCAL:
        switch (b->local.type) {
          case BT_LET:
          case BT_FN_ARG:
          case BT_FN_REF:
            break;
          default:
            throwCompilerError(error, "unsupported: %u", b->local.type);
        }
        break;

      case BS_CAPTURED:
        break;

      default:
        throwCompilerError(error, "unsupported: %u", b->source);
    }
  }

  // store all the arguments in slots
  for (uint16_t i=0; i<bindingTable->usedSpace; i++) {
    Binding *b = &bindingTable->bindings[i];
    if (b->source == BS_LOCAL && b->local.type == BT_FN_ARG) {
      slotsTable[i] = slotsCounter;
      slotsCounter++;
    }
  }

  // store captured bindings in slots
  for (uint16_t i=0; i<bindingTable->usedSpace; i++) {
    Binding *b = &bindingTable->bindings[i];
    if (b->source == BS_CAPTURED) {
      slotsTable[i] = slotsCounter;
      slotsCounter++;
    }
  }

  // if the function defines a self-referential binding, store that in a slot, there can be only one
  bool foundFnRef = false;
  for (uint16_t i=0; i<bindingTable->usedSpace; i++) {
    Binding *b = &bindingTable->bindings[i];
    if (b->source == BS_LOCAL && b->local.type == BT_FN_REF) {
      if (foundFnRef) {
        throwCompilerError(error, "unsupported: %u", b->source);
      }
      slotsTable[i] = slotsCounter;
      slotsCounter++;
      foundFnRef = true;
    }
  }

  // the remaining slots are used for let-bindings
  for (uint16_t i=0; i<bindingTable->usedSpace; i++) {
    Binding *b = &bindingTable->bindings[i];
    if (b->source == BS_LOCAL && b->local.type == BT_LET) {
      slotsTable[i] = slotsCounter;
      slotsCounter++;
    }
  }

  *ptr = slotsTable;
  return R_SUCCESS;

  failure:
    return ret;
}

uint64_t computeOpStackSize(uint8_t *code, uint16_t length) {

  uint16_t maxOpStack = 0;
  uint16_t currentOpStack = 0;

  void *end = (void*)code + (length * sizeof(uint8_t));

  while ( ((void*)code) < end) {
    uint8_t inst = *code;
    switch (inst) {
      case I_LOAD_CONST:
      case I_LOAD_LOCAL:
        code += 3;
        currentOpStack++;
        break;
      case I_STORE_LOCAL:
        code += 3;
        currentOpStack--;
        break;
      case I_INVOKE_DYN: {
        code++;
        uint16_t numArgs = (code[0] << 8) | code[1];
        currentOpStack -= numArgs;
        code += 2;
        break;
      }
      case I_INVOKE_DYN_TAIL: {
        code++;
        uint16_t numArgs = (code[0] << 8) | code[1];
        currentOpStack -= numArgs;
        code += 2;
        // history ends for stack
        break;
      }
      case I_RET:
        code++;
        currentOpStack--;
        break;
      case I_CMP:
        code++;
        currentOpStack--;
        break;
      case I_JMP:
        code += 3;
        break;
      case I_JMP_IF:
        code += 3;
        currentOpStack--;
        break;
      case I_JMP_IF_NOT:
        code += 3;
        currentOpStack--;
        break;
      case I_ADD:
        code++;
        currentOpStack--;
        break;
      case I_SUB:
        code++;
        currentOpStack--;
        break;
      case I_DEF_VAR:
        code += 3;
        break;
      case I_LOAD_VAR:
        code += 3;
        currentOpStack++;
        break;
      case I_LOAD_CLOSURE:
        code += 3;
        uint16_t numCaptured = (code[0] << 8) | code[1];
        code += 2;
        currentOpStack++; // load
        currentOpStack -= numCaptured;
        break;
      case I_SWAP:
        code++;
        break;
      case I_PUSH_HANDLER:
        code += 3;
        currentOpStack -= 1;
        break;
      case I_POP_HANDLER:
        code++;
        break;
      case I_CONS:
        code++;
        currentOpStack--;
        break;
      default:
        explode("oops");
    }

    if (currentOpStack > maxOpStack) {
      maxOpStack = currentOpStack;
    }
  }

  return maxOpStack;
}

RetVal tryCompileFnConstant(Form *form, Output output, Error *error) {
  RetVal ret;

  // clean up on failure
  Constants fnConstants;
  Codes fnCodes;
  LineNumbers lineNumbers;

  // clean up always
  uint16_t *slotsTable = NULL;

  constantsInitContents(&fnConstants);
  codesInitContents(&fnCodes);
  lineNumbersInitContents(&lineNumbers);

  throws(trySlotsTableBuild(output.pool, &form->fn.table, &slotsTable, error));

  {
    Output fnOutput;
    fnOutput.pool = output.pool;
    fnOutput.constants = &fnConstants;
    fnOutput.codes = &fnCodes;
    fnOutput.slotsTable = slotsTable;
    fnOutput.lineNumbers = &lineNumbers;
    fnOutput.hasFileName = output.hasFileName;
    fnOutput.fileName = output.fileName;

    for (uint16_t i = 0; i < form->fn.forms.numForms; i++) {
      throws(tryCompile(&form->fn.forms.forms[i], fnOutput, error));
    }

    uint8_t retCode[] = { I_RET };
    throws(tryCodeAppend(fnOutput, sizeof(retCode), retCode, error));
  }

  FnConstant fnConst;
  constantFnInitContents(&fnConst);

  fnConst.hasName = form->fn.hasName;
  fnConst.name = NULL;
  if (fnConst.hasName) {
    throws(tryCopyText(output.pool, form->fn.name.value, &fnConst.name, form->fn.name.length, error));
  }
  fnConst.numArgs = form->fn.numArgs;
  fnConst.usesVarArgs = form->fn.usesVarArgs;
  fnConst.numConstants = fnConstants.numUsed;
  fnConst.constants = fnConstants.constants;
  fnConst.code.numLocals = form->fn.table.usedSpace;
  fnConst.code.maxOperandStackSize = computeOpStackSize(fnCodes.codes, fnCodes.numUsed);
  fnConst.code.codeLength = fnCodes.numUsed;
  fnConst.code.code = fnCodes.codes;

  if (output.hasFileName) {
    fnConst.code.hasSourceTable = true;

    SourceTable *table = &fnConst.code.sourceTable;
    sourceTableInitContents(table);
    throws(tryCopyText(output.pool, output.fileName.value, &table->fileName, output.fileName.length, error));
    table->numLineNumbers = lineNumbers.numUsed;
    table->lineNumbers = lineNumbers.numbers;
  }

  Constant c;
  c.type = CT_FN;
  c.function = fnConst;
  throws(tryAppendConstant(output, c, error));

  return R_SUCCESS;
  failure:
    return ret;
}


RetVal tryCompileFn(Form *form, Output output, Error *error) {
  RetVal ret;

  throws(tryCompileFnConstant(form, output, error));
  uint16_t fnConstIndex = output.constants->numUsed - 1;

  if (form->fn.isClosure) {

    for (uint16_t i=0; i<form->fn.table.usedSpace; i++) {
      Binding *b = &form->fn.table.bindings[i];
      if (b->source == BS_CAPTURED) {
        uint16_t slotIndex = output.slotsTable[b->captured.bindingIndex];
        uint8_t code[] = { I_LOAD_LOCAL, slotIndex >> 8, slotIndex & 0xFF };
        throws(tryCodeAppend(output, sizeof(code), code, error));
      }
    }

    uint8_t loadInst = I_LOAD_CLOSURE;
    uint8_t code[] = {
        loadInst,
        fnConstIndex >> 8, fnConstIndex & 0xFF,
        form->fn.numCaptures >> 8, form->fn.numCaptures & 0xFF
    };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }
  else {
    uint8_t loadInst = I_LOAD_CONST;
    uint8_t code[] = { loadInst, fnConstIndex >> 8, fnConstIndex & 0xFF };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal nilConstantGetIndex(Output output, uint16_t *index, Error *error) {
  RetVal ret;

  // look for already-defined nil ref constant
  for (uint16_t i=0; i<output.constants->numUsed; i++) {
    Constant *c = &output.constants->constants[i];
    if (c->type == CT_NIL) {
      *index = i;
      return R_SUCCESS;
    }
  }

  // create nil constant
  Constant c;
  c.type = CT_NIL;
  throws(tryAppendConstant(output, c, error));
  *index = output.constants->numUsed - 1;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal uintConstantGetIndex(uint64_t value, Output output, uint16_t *index, Error *error) {
  RetVal ret;

  // look for already-defined uint constant
  for (uint16_t i=0; i<output.constants->numUsed; i++) {
    Constant *c = &output.constants->constants[i];
    if (c->type == CT_INT && c->integer == value) {
      *index = i;
      return R_SUCCESS;
    }
  }

  // create int constant
  Constant c;
  c.type = CT_INT;
  c.integer = value;
  throws(tryAppendConstant(output, c, error));
  *index = output.constants->numUsed - 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal varRefConstantGetIndex(Text name, Output output, uint16_t *index, Error *error) {
  RetVal ret;

  // look for already-defined var ref constant
  for (uint16_t i=0; i<output.constants->numUsed; i++) {
    Constant *c = &output.constants->constants[i];
    if (c->type == CT_SYMBOL && wcscmp(c->symbol.value, name.value) == 0) {
      *index = i;
      return R_SUCCESS;
    }
  }

  // create or reuse var ref constant
  Constant c;
  c.type = CT_SYMBOL;
  c.symbol.length = name.length;
  throws(tryCopyText(output.pool, name.value, &c.symbol.value, c.symbol.length, error));
  throws(tryAppendConstant(output, c, error));
  *index = output.constants->numUsed - 1;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal appendMeta(Output output, Expr *constant, ConstantMeta *meta, Error *error) {
  RetVal ret;

  constantMetaInit(meta);
  if (constant->source.isSet) {

    if (constant->source.lineNumber == 0) {
      throwRuntimeError(error, "line number is required if source has been set");
    }

    meta->numProperties = 1;
    tryPalloc(output.pool, meta->properties, sizeof(ConstantMetaProperty) * meta->numProperties,
        "ConstantMetaProperty array");
    ConstantMetaProperty *lineNo = &meta->properties[0];

    {
      wchar_t *keyName = L"line-number";
      Constant key;
      key.type = CT_KEYWORD;
      key.keyword.length = wcslen(keyName);
      throws(tryCopyText(output.pool, keyName, &key.keyword.value, key.keyword.length, error));
      throws(tryAppendConstant(output, key, error));
      uint16_t index = output.constants->numUsed - 1;
      lineNo->keyIndex = index;
    }

    throws(uintConstantGetIndex(constant->source.lineNumber, output, &lineNo->valueIndex, error));
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal constInitContents(Expr *constant, Constant *c, Output output, Error *error) {
  RetVal ret;

  switch (constant->type) {

    case N_NUMBER: {
      c->type = CT_INT;
      c->integer = constant->number.value;
      break;
    }
    case N_NIL: {
      c->type = CT_NIL;
      break;
    }
    case N_BOOLEAN: {
      c->type = CT_BOOL;
      c->boolean = (uint8_t) constant->boolean.value;
      break;
    }
    case N_STRING: {
      c->type = CT_STR;
      c->string.length = constant->string.length;
      throws(tryCopyText(output.pool, constant->string.value, &c->string.value, c->string.length, error));
      break;
    }
    case N_SYMBOL: {
      c->type = CT_SYMBOL;
      c->symbol.length = constant->symbol.length;
      throws(tryCopyText(output.pool, constant->symbol.value, &c->symbol.value, c->symbol.length, error));
      break;
    }
    case N_KEYWORD: {
      c->type = CT_KEYWORD;
      c->keyword.length = constant->keyword.length;
      throws(tryCopyText(output.pool, constant->keyword.value, &c->keyword.value, c->keyword.length, error));
      break;
    }
    case N_LIST: {
      c->type = CT_LIST;
      c->list.length = constant->list.length;
      tryPalloc(output.pool, c->list.constants, sizeof(uint16_t) * c->list.length, "constant list array");

      throws(appendMeta(output, constant, &c->list.meta, error));

      ListElement *elem = constant->list.head;
      uint16_t elemIndex = 0;
      while (elem != NULL) {

        Constant child;
        throws(constInitContents(elem->expr, &child, output, error));
        throws(tryAppendConstant(output, child, error));

        uint16_t childIndex = output.constants->numUsed - 1;
        c->list.constants[elemIndex] = childIndex;

        elem = elem->next;
        elemIndex = elemIndex + 1;
      }

      break;
    }
    case N_VEC: {
      c->type = CT_VEC;
      c->vec.length = constant->vec.length;
      tryPalloc(output.pool, c->vec.constants, sizeof(uint16_t) * c->vec.length, "constant vec array");

      ListElement *elem = constant->vec.head;
      uint16_t elemIndex = 0;
      while (elem != NULL) {

        Constant child;
        throws(constInitContents(elem->expr, &child, output, error));
        throws(tryAppendConstant(output, child, error));

        uint16_t childIndex = output.constants->numUsed - 1;
        c->vec.constants[elemIndex] = childIndex;

        elem = elem->next;
        elemIndex = elemIndex + 1;
      }

      break;
    }
    case N_MAP: {

      c->type = CT_MAP;
      c->map.length = constant->map.length;
      tryPalloc(output.pool, c->map.constants, sizeof(uint16_t) * c->map.length * 2, "constant map array");

      throws(appendMeta(output, constant, &c->map.meta, error));

      MapElement *elem = constant->map.head;
      uint16_t elemIndex = 0;
      while (elem != NULL) {

        Constant key, value;

        throws(constInitContents(elem->key, &key, output, error));
        throws(tryAppendConstant(output, key, error));
        uint16_t keyIndex = output.constants->numUsed - 1;
        c->map.constants[elemIndex] = keyIndex;

        throws(constInitContents(elem->value, &value, output, error));
        throws(tryAppendConstant(output, value, error));
        uint16_t valueIndex = output.constants->numUsed - 1;
        c->map.constants[elemIndex + 1] = valueIndex;

        elem = elem->next;
        elemIndex = elemIndex + 2;
      }

      break;
    }
    default:
      throwCompilerError(error, "unsupported: %u", constant->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileConst(Form *form, Output output, Error *error) {
  RetVal ret;

  Constant c;

  throws(constInitContents(form->constant, &c, output, error));
  throws(tryAppendConstant(output, c, error));

  uint16_t index = output.constants->numUsed - 1;
  uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileDef(Form *form, Output output, Error *error) {
  RetVal ret;

  if (form->def.value == NULL) {
    uint16_t index;
    throws(nilConstantGetIndex(output, &index, error));
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }
  else {
    throws(tryCompile(form->def.value, output, error));
  }

  uint16_t index;
  throws(varRefConstantGetIndex(form->def.name, output, &index, error));

  uint8_t code[] = { I_DEF_VAR, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileVarRef(Form *form, Output output, Error *error) {
  RetVal ret;

  uint16_t index;
  throws(varRefConstantGetIndex(form->varRef.name, output, &index, error));

  uint8_t code[] = { I_LOAD_VAR, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileLet(Form *form, Output output, Error *error) {
  RetVal ret;

  // 'let's add locals to the current frame

  for (uint16_t i=0; i<form->let.numBindings; i++) {
    LetBinding *binding = &form->let.bindings[i];

    throws(tryCompile(binding->value, output, error));

    uint16_t index = output.slotsTable[binding->bindingIndex];
    uint8_t code[] = { I_STORE_LOCAL, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }

  if (form->let.forms.numForms > 0) {
    for (uint16_t i = 0; i < form->let.forms.numForms; i++) {
      Form *f = &form->let.forms.forms[i];
      throws(tryCompile(f, output, error));
    }
  }
  else {
    Constant c;
    c.type = CT_NIL;
    throws(tryAppendConstant(output, c, error));

    uint16_t index = output.constants->numUsed - 1;
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileEnvRef(Form *form, Output output, Error *error) {
  RetVal ret;

  uint16_t index = output.slotsTable[form->envRef.bindingIndex];
  uint8_t code[] = { I_LOAD_LOCAL, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileBuiltin(Form *form, Output output, Error *error) {
  RetVal ret;

  FormBuiltin *builtin = &form->builtin;

  if (wcscmp(builtin->name.value, L"add") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_ADD};
    throws(tryCodeAppend(output, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"subtract") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_SUB };
    throws(tryCodeAppend(output, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"compare") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_CMP };
    throws(tryCodeAppend(output, sizeof(addCode), addCode, error));
  }
  else {
    throwCompilerError(error, "unsupported builtin '%ls'", builtin->name.value);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileFnCall(Form *form, Output output, Error *error) {
  RetVal ret;

  // push the arguments in evaluation (left-to-right) order
  for (uint16_t i = 0; i<form->fnCall.args.numForms; i++) {
    throws(tryCompile(&form->fnCall.args.forms[i], output, error));
  }

  // push the callable
  throws(tryCompile(form->fnCall.fnCallable, output, error));

  // invoke

  uint8_t inst;
  if (form->fnCall.tailPosition) {
    inst = I_INVOKE_DYN_TAIL;
  }
  else {
    inst = I_INVOKE_DYN;
  }
  uint16_t numForms = form->fnCall.args.numForms;
  uint8_t code[] = { inst, numForms >> 8, numForms & 0xFF };
  throws(tryCodeAppend(output, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileList(Form *form, Output output, Error *error) {
  RetVal ret;

  Forms forms = form->list.forms;

  if (forms.numForms == 0) {
    {
      Constant c;
      c.type = CT_NIL;
      throws(tryAppendConstant(output, c, error));
      uint16_t index = output.constants->numUsed - 1;
      uint8_t code[] = {I_LOAD_CONST, index >> 8, index & 0xFF};
      throws(tryCodeAppend(output, sizeof(code), code, error));
    }
  }
  else {
    for (int i=0; i < forms.numForms; i++) {

      uint16_t idx = forms.numForms - (i + 1);
      Form *f = &forms.forms[idx];

      if (i == 0) {
        throws(tryCompile(f, output, error));

        Constant c;
        c.type = CT_NIL;
        throws(tryAppendConstant(output, c, error));
        uint16_t index = output.constants->numUsed - 1;
        uint8_t code[] = {I_LOAD_CONST, index >> 8, index & 0xFF};
        throws(tryCodeAppend(output, sizeof(code), code, error));

        uint8_t addCode[] = {I_CONS};
        throws(tryCodeAppend(output, sizeof(addCode), addCode, error));
      }
      else {
        throws(tryCompile(f, output, error));
        uint8_t addCode[] = {I_SWAP, I_CONS};
        throws(tryCodeAppend(output, sizeof(addCode), addCode, error));
      }
    }
  }

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryCompileHandler(Form *form, Output output, Error *error) {
  RetVal ret;

  throws(tryCompile(form->handler.handler, output, error));

  // emit the pushHandler code, keep a pointer to the jump address
  uint16_t jumpAddrOffset;
  {
    uint8_t code[] = { I_PUSH_HANDLER, 0, 0 };
    throws(tryCodeAppend(output, sizeof(code), code, error));
    jumpAddrOffset = output.codes->numUsed - 2;
  }

  // emit all the forms
  Forms forms = form->handler.forms;
  if (forms.numForms == 0) {
    {
      Constant c;
      c.type = CT_NIL;
      throws(tryAppendConstant(output, c, error));
      uint16_t index = output.constants->numUsed - 1;
      uint8_t code[] = {I_LOAD_CONST, index >> 8, index & 0xFF};
      throws(tryCodeAppend(output, sizeof(code), code, error));
    }
  }
  else {
    for (int i=0; i < forms.numForms; i++) {
      Form *f = &form->handler.forms.forms[i];
      throws(tryCompile(f, output, error));
    }
  }

  // the jumpAddrOffset should point to the first address after the forms
  {
    uint16_t nextCodeAddr = output.codes->numUsed;
    setIndexAtOffset(output, jumpAddrOffset, nextCodeAddr);
  }

  // remove the handler
  uint8_t code[] = { I_POP_HANDLER };
  throws(tryCodeAppend(output, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryCompile(Form *form, Output output, Error *error) {
  RetVal ret;

  throws(tryAppendSource(form, output, error));

  switch (form->type) {

    case F_CONST:
      throws(tryCompileConst(form, output, error));
      break;

    case F_IF:
      throws(tryCompileIf(form, output, error));
      break;

    case F_DEF:
      throws(tryCompileDef(form, output, error));
      break;

    case F_VAR_REF:
      throws(tryCompileVarRef(form, output, error));
      break;

    case F_LET:
      throws(tryCompileLet(form, output, error));
      break;

    case F_ENV_REF:
      throws(tryCompileEnvRef(form, output, error));
      break;

    case F_BUILTIN:
      throws(tryCompileBuiltin(form, output, error));
      break;

    case F_FN:
      throws(tryCompileFn(form, output, error));
      break;

    case F_FN_CALL:
      throws(tryCompileFnCall(form, output, error));
      break;

    case F_LIST:
      throws(tryCompileList(form, output, error));
      break;

    case F_HANDLER:
      throws(tryCompileHandler(form, output, error));
      break;

    default:
      throwCompilerError(error, "unsupported");
      break;
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileTopLevel(Pool_t pool, FormRoot *root, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Constants constants;
  Codes codes;
  uint16_t *slotsTable;
  LineNumbers lineNumbers;

  constantsInitContents(&constants);
  codesInitContents(&codes);
  codeUnitInitContents(codeUnit);
  slotsTable = NULL;
  lineNumbersInitContents(&lineNumbers);

  throws(trySlotsTableBuild(pool, &root->table, &slotsTable, error));

  {
    Output output;
    output.pool = pool;
    output.constants = &constants;
    output.codes = &codes;
    output.slotsTable = slotsTable;
    output.lineNumbers = &lineNumbers;
    output.hasFileName = root->hasFileName;
    output.fileName = root->fileName;

    throws(tryCompile(root->form, output, error));

    uint8_t code[] = { I_RET };
    throws(tryCodeAppend(output, sizeof(code), code, error));
  }

  codeUnit->code.numLocals = root->table.usedSpace;
  codeUnit->numConstants = constants.numUsed;
  codeUnit->constants = constants.constants;
  codeUnit->code.codeLength = codes.numUsed;
  codeUnit->code.code = codes.codes;

  /*
   * TODO: finish populating the source table
   *
   * When emitting code for a form that has source location info, append LineNumbers to the table
   * Remove the duplicate LineNumbers
   * This has to be done for all the compile functions
   */
  if (root->hasFileName) {
    codeUnit->code.hasSourceTable = true;

    SourceTable *table = &codeUnit->code.sourceTable;
    sourceTableInitContents(table);

    throws(tryCopyText(pool, root->fileName.value, &table->fileName, root->fileName.length, error));
    table->numLineNumbers = lineNumbers.numUsed;
    table->lineNumbers = lineNumbers.numbers;
  }

  codeUnit->code.maxOperandStackSize = computeOpStackSize(codeUnit->code.code, codeUnit->code.codeLength);

  return R_SUCCESS;
  failure:
    return ret;
}



