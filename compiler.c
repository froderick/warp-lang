#include "compiler.h"

// utilities for accumulating constants and code as the result of compilation

typedef struct Constants {
  uint16_t numAllocated;
  uint16_t numUsed;
  Constant *constants;
} Constants;

void constantsInitContents(Constants *constants) {
  constants->numAllocated = 0;
  constants->numUsed = 0;
  constants->constants = NULL;
}

void constantsFreeContents(Constants *constants) {
  if (constants != NULL) {
    constants->numAllocated = 0;
    constants->numUsed = 0;
    if (constants->constants != NULL) {
      for (int i=0; i<constants->numUsed; i++) {
        _constantFreeContents(&constants->constants[i]);
      }
      free(constants->constants);
      constants->constants = NULL;
    }
  }
}

RetVal tryAppendConstant(Constants *constants, Constant c, Error *error) {
  RetVal ret;

  if (constants->constants == NULL) {
    uint16_t len = 16;
    tryMalloc(constants->constants, len * sizeof(Constant), "Constant array");
    constants->numAllocated = len;
  }
  else if (constants->numUsed == constants->numAllocated) {
    uint16_t newAllocatedLength = constants->numAllocated * 2;

    Constant *resizedConstants = realloc(constants->constants, sizeof(Constant) * newAllocatedLength);
    if (resizedConstants == NULL) {
      ret = memoryError(error, "realloc Constant array");
      goto failure;
    }

    constants->numAllocated = newAllocatedLength;
    constants->constants = resizedConstants;
  }

  uint16_t index = constants->numUsed;
  constants->constants[index] = c;
  constants->numUsed = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

typedef struct Codes {
  uint16_t numAllocated;
  uint16_t numUsed;
  uint8_t *codes;
} Codes;

void codesInitContents(Codes *codes) {
  codes->numAllocated = 0;
  codes->numUsed = 0;
  codes->codes = NULL;
}

void codesFreeContents(Codes *codes) {
  if (codes != NULL) {
    codes->numAllocated = 0;
    codes->numUsed = 0;
    if (codes->codes!= NULL) {
      free(codes->codes);
      codes->codes = NULL;
    }
  }
}

RetVal tryCodeAppend(Codes *codes, uint16_t numAdded, uint8_t *added, Error *error) {
  RetVal ret;

  if (codes->codes == NULL) {
    uint16_t len = 16;
    tryMalloc(codes->codes, len * sizeof(uint8_t), "byte array");
    codes->numAllocated = len;
  }
  else if (codes->numUsed + numAdded < codes->numAllocated) {
    uint16_t newAllocatedLength = (codes->numAllocated + numAdded) * 2;

    uint8_t *resized = realloc(codes->codes, newAllocatedLength * sizeof(uint8_t));
    if (resized == NULL) {
      ret = memoryError(error, "realloc Constant array");
      goto failure;
    }

    codes->numAllocated = newAllocatedLength;
    codes->codes = resized;
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

RetVal tryCodeAppendContents(Codes *from, Codes *to, Error *error) {
  RetVal ret;

  throws(tryCodeAppend(to, from->numUsed, from->codes, error));
  return R_SUCCESS;

  failure:
    return ret;
}

void codesClear(Codes *codes) {
  codes->numUsed = 0;
}

// compiler behavior that emits constants and code

typedef struct Output {
  Constants *constants;
  Codes *codes;
  uint16_t *slotsTable; // maps binding table indexes to slot indexes for storing locals
} Output;

RetVal tryCompile(Form *form, Output output, Error *error);

RetVal tryCompileIf(Form *form, Output output, Error *error) {
  RetVal ret;

  Codes ifBranch, elseBranch;

  codesInitContents(&ifBranch);
  codesInitContents(&elseBranch);

  // emit the test in the main code space
  throws(tryCompile(form->iff.test, output, error));

  // emit ifBranch form in temporary code space
  {
    Output ifOutput = output;
    ifOutput.codes = &ifBranch;
    throws(tryCompile(form->iff.ifBranch, ifOutput, error));
  }

  if (form->iff.elseBranch == NULL) {

    // emit test
    // emit I_JMP_IF_NOT $END_ADDR
    // emit ifBranch {
    //  - do stuff
    // }
    // $END_ADDR

    // compute the conditional code, lengths and offsets
    uint16_t nextCodeAddr = output.codes->numUsed;
    uint16_t ifBranchLength = ifBranch.numUsed;
    uint16_t jumpToEndAddr = nextCodeAddr + ifBranchLength;
    uint8_t jumpToEndCode[] = { I_JMP_IF_NOT, jumpToEndAddr >> 8, jumpToEndAddr & 0xFF };

    // emit the code for real
    throws(tryCodeAppend(output.codes, sizeof(jumpToEndCode), jumpToEndCode, error));
    throws(tryCodeAppendContents(&ifBranch, output.codes, error));
  }
  else {

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

    // emit elseBranch form in temporary code space
    {
      Output elseOutput = output;
      elseOutput.codes = &elseBranch;
      throws(tryCompile(form->iff.elseBranch, elseOutput, error));
    }

    // compute the conditional code, lengths and offsets
    uint16_t nextCodeAddr = output.codes->numUsed;
    uint16_t jumpToElseCodeLength = 3;
    uint16_t ifBranchLength = ifBranch.numUsed;
    uint16_t jumpToEndCodeLength = 3;
    uint16_t elseBranchLength = elseBranch.numUsed;

    uint16_t jumpToElseAddr = nextCodeAddr + jumpToElseCodeLength + ifBranchLength + jumpToEndCodeLength;
    uint8_t jumpToElseCode[] = {I_JMP_IF_NOT, jumpToElseAddr >> 8, jumpToElseAddr & 0xFF};

    uint16_t jumpToEndAddr = jumpToElseAddr + elseBranchLength;
    uint8_t jumpToEndCode[] = {I_JMP, jumpToEndAddr >> 8, jumpToEndAddr & 0xFF};

    // emit the code for real
    throws(tryCodeAppend(output.codes, sizeof(jumpToElseCode), jumpToElseCode, error));
    throws(tryCodeAppendContents(&ifBranch, output.codes, error));
    throws(tryCodeAppend(output.codes, sizeof(jumpToEndCode), jumpToEndCode, error));
    throws(tryCodeAppendContents(&elseBranch, output.codes, error));
  }

  codesFreeContents(&ifBranch);
  codesFreeContents(&elseBranch);

  return R_SUCCESS;

  failure:
    codesFreeContents(&ifBranch);
    codesFreeContents(&elseBranch);
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

RetVal trySlotsTableBuild(BindingTable *bindingTable, uint16_t **ptr, Error *error) {
  RetVal ret;

  uint16_t *slotsTable = NULL;

  tryMalloc(slotsTable, sizeof(uint16_t) * bindingTable->usedSpace, "uint16_t array");

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
    if (slotsTable != NULL) {
      free(slotsTable);
    }
    return ret;
}

RetVal tryCompileFnConstant(Form *form, Output output, Error *error) {
  RetVal ret;

  // clean up on failure
  Constants fnConstants;
  Codes fnCodes;

  // clean up always
  uint16_t *slotsTable = NULL;

  constantsInitContents(&fnConstants);
  codesInitContents(&fnCodes);

  {
    throws(trySlotsTableBuild(&form->fn.table, &slotsTable, error));

    Output fnOutput;
    fnOutput.constants = &fnConstants;
    fnOutput.codes = &fnCodes;
    fnOutput.slotsTable = slotsTable;

    // create fn ref constant, emit code to load it and store it at local[0]
    if (form->fn.hasName) {

      Constant c;
      c.type = CT_FN_REF;
      c.fnRef.fnId = form->fn.id;
      throws(tryAppendConstant(fnOutput.constants, c, error));

      uint16_t constantIndex = fnOutput.constants->numUsed - 1;
      uint16_t slotIndex = fnOutput.slotsTable[form->fn.bindingIndex];

      uint8_t code[] = { I_LOAD_CONST,  constantIndex >> 8, constantIndex & 0xFF,
                         I_STORE_LOCAL, slotIndex >> 8,     slotIndex & 0xFF };
      throws(tryCodeAppend(fnOutput.codes, sizeof(code), code, error));
    }

    for (uint16_t i = 0; i < form->fn.forms.numForms; i++) {
      throws(tryCompile(&form->fn.forms.forms[i], fnOutput, error));
    }
  }

  uint8_t retCode[] = { I_RET };
  throws(tryCodeAppend(&fnCodes, sizeof(retCode), retCode, error));

  FnConstant fnConst;
  constantFnInitContents(&fnConst);

  fnConst.fnId = form->fn.id;
  fnConst.numArgs = form->fn.numArgs;
  fnConst.numConstants = fnConstants.numUsed;
  fnConst.numCaptures = form->fn.numCaptures;
  fnConst.constants = fnConstants.constants;
  fnConst.code.numLocals = form->fn.table.usedSpace;
  fnConst.code.maxOperandStackSize = 100; // TODO: need to compute this
  fnConst.code.codeLength = fnCodes.numUsed;
  fnConst.code.code = fnCodes.codes;
  fnConst.code.hasSourceTable = false;

  Constant c;
  c.type = CT_FN;
  c.function = fnConst;
  throws(tryAppendConstant(output.constants, c, error));

  free(slotsTable);

  return R_SUCCESS;

  failure:
    constantsFreeContents(&fnConstants);
    codesFreeContents(&fnCodes);
    if (slotsTable != NULL) {
      free(slotsTable);
    }
    return ret;
}


RetVal tryCompileFn(Form *form, Output output, Error *error) {
  RetVal ret;

  throws(tryCompileFnConstant(form, output, error));
  uint16_t fnConstIndex = output.constants->numUsed - 1;

  uint8_t loadInst;
  if (form->fn.isClosure) {

    for (uint16_t i=0; i<form->fn.table.usedSpace; i++) {
      Binding *b = &form->fn.table.bindings[i];
      if (b->source == BS_CAPTURED) {
        uint16_t slotIndex = output.slotsTable[b->captured.bindingIndex];
        uint8_t code[] = { I_LOAD_LOCAL, slotIndex >> 8, slotIndex & 0xFF };
        throws(tryCodeAppend(output.codes, sizeof(code), code, error));
      }
    }

    loadInst = I_LOAD_CLOSURE;
  }
  else {
    loadInst = I_LOAD_CONST;
  }

  uint8_t code[] = { loadInst, fnConstIndex >> 8, fnConstIndex & 0xFF };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

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
  throws(tryAppendConstant(output.constants, c, error));
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
    if (c->type == CT_VAR_REF && wcscmp(c->varRef.name, name.value) == 0) {
      *index = i;
      return R_SUCCESS;
    }
  }

  // create or reuse var ref constant
  Constant c;
  c.type = CT_VAR_REF;
  c.varRef.nameLength = name.length;
  throws(tryCopyText(name.value, &c.varRef.name, c.varRef.nameLength, error));
  throws(tryAppendConstant(output.constants, c, error));
  *index = output.constants->numUsed - 1;

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
      throws(tryCopyText(constant->string.value, &c->string.value, c->string.length, error));
      break;
    }
    case N_SYMBOL: {
      c->type = CT_SYMBOL;
      c->symbol.length = constant->symbol.length;
      throws(tryCopyText(constant->symbol.value, &c->symbol.value, c->symbol.length, error));
      break;
    }
    case N_KEYWORD: {
      c->type = CT_KEYWORD;
      c->keyword.length = constant->keyword.length;
      throws(tryCopyText(constant->keyword.value, &c->keyword.value, c->keyword.length, error));
      break;
    }
    case N_LIST: {
      c->type = CT_LIST;
      c->list.length = constant->list.length;
      tryMalloc(c->list.constants, sizeof(uint16_t) * c->list.length, "typeIndex array");

      ListElement *elem = constant->list.head;
      uint16_t elemIndex = 0;
      while (elem != NULL) {

        Constant child;
        throws(constInitContents(elem->expr, &child, output, error));
        throws(tryAppendConstant(output.constants, child, error));

        uint16_t childIndex = output.constants->numUsed - 1;
        c->list.constants[elemIndex] = childIndex;

        elem = elem->next;
        elemIndex = elemIndex + 1;
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
  throws(tryAppendConstant(output.constants, c, error));

  uint16_t index = output.constants->numUsed - 1;
  uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };

  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    _constantFreeContents(&c);
    return ret;
}

RetVal tryCompileDef(Form *form, Output output, Error *error) {
  RetVal ret;

  if (form->def.value == NULL) {
    uint16_t index;
    throws(nilConstantGetIndex(output, &index, error));
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output.codes, sizeof(code), code, error));
  }
  else {
    throws(tryCompile(form->def.value, output, error));
  }

  uint16_t index;
  throws(varRefConstantGetIndex(form->def.name, output, &index, error));

  uint8_t code[] = { I_DEF_VAR, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileVarRef(Form *form, Output output, Error *error) {
  RetVal ret;

  uint16_t index;
  throws(varRefConstantGetIndex(form->varRef.name, output, &index, error));

  uint8_t code[] = { I_LOAD_VAR, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

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
    throws(tryCodeAppend(output.codes, sizeof(code), code, error));
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
    throws(tryAppendConstant(output.constants, c, error));

    uint16_t index = output.constants->numUsed - 1;
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output.codes, sizeof(code), code, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileEnvRef(Form *form, Output output, Error *error) {
  RetVal ret;

  uint16_t index = output.slotsTable[form->envRef.bindingIndex];
  uint8_t code[] = { I_LOAD_LOCAL, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

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
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"subtract") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_SUB };
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"compare") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_CMP };
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"first") == 0) {

    if (builtin->args.numForms!= 1) {
      throwCompilerError(error, "first takes only one argument, got %u", builtin->args.numForms);
    }

    throws(tryCompile(&form->builtin.args.forms[0], output, error));

    uint8_t addCode[] = { I_FIRST };
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"rest") == 0) {

    if (builtin->args.numForms!= 1) {
      throwCompilerError(error, "rest takes only one argument, got %u", builtin->args.numForms);
    }

    throws(tryCompile(&form->builtin.args.forms[0], output, error));

    uint8_t addCode[] = { I_REST };
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"cons") == 0) {

    if (builtin->args.numForms!= 2) {
      throwCompilerError(error, "cons takes two arguments, got %u", builtin->args.numForms);
    }

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_CONS };
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else if (wcscmp(builtin->name.value, L"setmacro") == 0) {

    if (builtin->args.numForms != 1) {
      throwCompilerError(error, "setmacro takes one argument, got %u", builtin->args.numForms);
    }

    for (int i=0; i < form->builtin.args.numForms; i++) {
      throws(tryCompile(&form->builtin.args.forms[i], output, error));
    }

    uint8_t addCode[] = { I_SET_MACRO };
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
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
  uint8_t code[] = { inst };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompile(Form *form, Output output, Error *error) {
  RetVal ret;

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

    case F_NONE:
      throwCompilerError(error, "unsupported");
      break;
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileTopLevel(FormRoot *root, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Constants constants;
  Codes codes;
  uint16_t *slotsTable;

  constantsInitContents(&constants);
  codesInitContents(&codes);
  codeUnitInitContents(codeUnit);
  slotsTable = NULL;

  throws(trySlotsTableBuild(&root->table, &slotsTable, error));

  {
    Output output;
    output.constants = &constants;
    output.codes = &codes;
    output.slotsTable = slotsTable;

    throws(tryCompile(root->form, output, error));

    uint8_t code[] = { I_RET };
    throws(tryCodeAppend(output.codes, sizeof(code), code, error));
  }

  codeUnit->code.numLocals = root->table.usedSpace;
  codeUnit->numConstants = constants.numUsed;
  codeUnit->constants = constants.constants;
  codeUnit->code.codeLength = codes.numUsed;
  codeUnit->code.code = codes.codes;

  // TODO: we don't populate these yet
  codeUnit->code.maxOperandStackSize = 10;
  codeUnit->code.hasSourceTable = false;

  free(slotsTable);

  return R_SUCCESS;

  failure:
    constantsFreeContents(&constants);
    codesFreeContents(&codes);
    if (slotsTable != NULL) {
      free(slotsTable);
    }
    return ret;
}



































/*
 * Thinking about macro expansion
 *
 * I don't want to write an interpreter, as well as a virtual machine and a compiler that compiles to it, just to be
 * able to execute macros at compile time. I'd rather write the compiler/vm and then do incremental compilation to
 * handle executing the macros at compile time.
 *
 * I'd do this by passing in a VM handle to the compiler as a parameter. The compiler can compile all forms, including
 * macros, and load them into the VM as it discovers them. Every form gets macro-expanded as a part of compilation.
 * When the compiler encounters a reference to a macro, it could take the arguments and feed them into a call to the
 * compiled macro inside the VM, and then use the result for compilation.
 *
 * TODO: think about this: one of the benefits of this model is that it allows us to handle resolving Vars with
 * // the actual virtual machine itself, rather than having to duplicate this in the compiler/analyzer itself.
 * // of course, this suggests that the var resolution should perhaps not be done in the analyzer at all...
 * // rather, the VarRef can just be the name of the symbol, unqualified. the compiler can inspect the symbols in
 * // the namespaces and do compile-time resolution based on what it finds in the virtual machine.
 */


// TODO: first, rest, cons
/*
 * loading the first field on an object via an object reference
 * cons - this can work by calling I_NEW
 */

// emit I_DUP so we have two copies of the arg on the stack
// emit I_TYPE_ASSERT to verify value type is a Cons
// emit I_LOAD_FIELD to load the first field from the Cons

// emit I_DUP so we have two copies of the arg on the stack
// emit I_TYPE_ASSERT to verify value type is a Cons
// emit I_LOAD_FIELD to load the second field from the Cons

/*
 * newcons
 * arg1
 * arg0
 */

// emit I_DUP so we have two copies of the second arg on the stack
// emit I_TYPE_ASSERT to verify the second arg's value type is a Cons
// emit I_NEW to create a new cons object
// emit I_DUP1
// emit I_STORE_FIELD to set the cons 'value' field with the first argument
// emit I_DUP1
// emit I_STORE_FIELD to set the cons 'next'field with the second argument

/*

 * the compiler taking knowlege of the record layout of Cons.
 *
 * TODO: are lists builtins the VM supplies, or things that are implemented on top of it?
 */
