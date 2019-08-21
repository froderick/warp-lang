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

void constantAppend(Output output, Constant c) {
  Constants *constants = output.constants;

  if (constants->constants == NULL) {
    uint16_t len = 16;
    palloc(output.pool, constants->constants, len * sizeof(Constant), "Constant array");
    constants->numAllocated = len;
  }
  else if (constants->numUsed == constants->numAllocated) {
    uint16_t newAllocatedLength = constants->numAllocated * 2;

    Constant *resized = NULL;
    palloc(output.pool, resized, newAllocatedLength * sizeof(Constant), "Constant array");
    memcpy(resized, constants->constants, constants->numUsed * sizeof(Constant));

    constants->numAllocated = newAllocatedLength;
    constants->constants = resized;
  }

  uint16_t index = constants->numUsed;
  constants->constants[index] = c;
  constants->numUsed = index + 1;
}

void codesInitContents(Codes *codes) {
  codes->numAllocated = 0;
  codes->numUsed = 0;
  codes->codes = NULL;
}

void codeAppend(Output output, uint16_t numAdded, uint8_t *added) {
  Codes *codes = output.codes;

  if (codes->codes == NULL) {
    uint16_t len = 16;
    palloc(output.pool, codes->codes, len * sizeof(uint8_t), "byte array");
    codes->numAllocated = len;
  }
  else if (codes->numUsed + numAdded > codes->numAllocated) {
    uint16_t newAllocatedLength = (codes->numAllocated + numAdded) * 2;

    uint8_t *replacement = NULL;
    palloc(output.pool, replacement, newAllocatedLength * sizeof(uint8_t), "byte array");
    memcpy(replacement, codes->codes, codes->numUsed * sizeof(uint8_t));

    codes->numAllocated = newAllocatedLength;
    codes->codes = replacement;
  }

  uint16_t baseIndex = codes->numUsed;
  for (uint16_t i=0; i<numAdded; i++) {
    codes->codes[baseIndex + i] = added[i];
  }
  codes->numUsed += numAdded;
}

void lineNumbersInitContents(LineNumbers *numbers) {
  numbers->numAllocated = 0;
  numbers->numUsed = 0;
  numbers->numbers = NULL;
}

void lineNumbersAppend(Pool_t pool, LineNumbers *numbers, LineNumber number) {
  if (numbers->numbers == NULL) {
    uint16_t len = 16;
    palloc(pool, numbers->numbers, len * sizeof(LineNumber), "LineNumber array");
    numbers->numAllocated = len;
  }
  else if (numbers->numUsed == numbers->numAllocated) {
    uint16_t newAllocatedLength = numbers->numAllocated * 2;

    LineNumber *resized = NULL;
    palloc(pool, resized, newAllocatedLength * sizeof(LineNumber), "LineNumber array");
    memcpy(resized, numbers->numbers, numbers->numUsed * sizeof(LineNumber));

    numbers->numAllocated = newAllocatedLength;
    numbers->numbers = resized;
  }

  uint16_t index = numbers->numUsed;
  numbers->numbers [index] = number;
  numbers->numUsed = index + 1;
}

// compiler behavior that emits constants and code

/*
 * If present, appends the source line number from the form to the
 * *next* code index to be appended.
 */
void sourceAppend(Form *form, Output output) {
  if (form->source.isSet) {

    if (form->source.lineNumber == 0) {
      explode("line number is zero");
    }

    LineNumber lineNumber;
    lineNumberInitContents(&lineNumber);
    lineNumber.lineNumber = form->source.lineNumber;
    lineNumber.startInstructionIndex = output.codes->numUsed;

    lineNumbersAppend(output.pool, output.lineNumbers, lineNumber);
  }
}

void setIndexAtOffset(Output output, uint16_t index, uint16_t value) {
  output.codes->codes[index] = value >> 8;
  output.codes->codes[index + 1] = value & 0xFF;
}

void compile(Form *form, Output output);

uint16_t nilConstantGetIndex(Output output);

void compileIf(Form *form, Output output) {
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
  compile(form->iff.test, output);

  // emit the testFailedJump code, keep a pointer to the jump address
  uint16_t testFailedJumpAddrOffset;
  {
    uint8_t testFailedJumpCode[] = { I_JMP_IF_NOT, 0, 0 };
    codeAppend(output, sizeof(testFailedJumpCode), testFailedJumpCode);
    testFailedJumpAddrOffset = output.codes->numUsed - 2;
  }

  // emit ifBranch form code
  compile(form->iff.ifBranch, output);

  // emit the jumpToEnd code to terminate the if branch, keep a pointer to the jump address
  uint16_t jumpToEndAddrOffset;
  {
    uint8_t jumpToEndCode[] = {I_JMP, 0, 0};
    codeAppend(output, sizeof(jumpToEndCode), jumpToEndCode);
    jumpToEndAddrOffset = output.codes->numUsed - 2;
  }

  // the testFailedJumpAddr should point to the first address after the ifBranch
  {
    uint16_t nextCodeAddr = output.codes->numUsed;
    setIndexAtOffset(output, testFailedJumpAddrOffset, nextCodeAddr);
  }

  // emit elseBranch form code
  if (form->iff.elseBranch != NULL) {
    compile(form->iff.elseBranch, output);
  }
  else {
    uint16_t index = nilConstantGetIndex(output);
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    codeAppend(output, sizeof(code), code);
  }

  // the jumpToEndAddr should point to the first address after the elseBranch
  {
    uint16_t nextCodeAddr = output.codes->numUsed;
    setIndexAtOffset(output, jumpToEndAddrOffset, nextCodeAddr);
  }
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

uint16_t* slotsTableBuild(Pool_t pool, BindingTable *bindingTable) {

  uint16_t *slotsTable = NULL;

  palloc(pool, slotsTable, sizeof(uint16_t) * bindingTable->usedSpace, "uint16_t array");

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
            explode("unsupported: %u", b->local.type);
        }
        break;

      case BS_CAPTURED:
        break;

      default:
        explode("unsupported: %u", b->source);
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
        explode("unsupported: %u", b->source);
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

  return slotsTable;
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
      case I_INVOKE_DYN_TAIL_RECURSE: {
        code++;
        uint16_t numArgs = (code[0] << 8) | code[1];
        currentOpStack -= numArgs;
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

void compileFnConstant(Form *form, Output output) {
  Constants fnConstants;
  Codes fnCodes;
  LineNumbers lineNumbers;

  constantsInitContents(&fnConstants);
  codesInitContents(&fnCodes);
  lineNumbersInitContents(&lineNumbers);

  uint16_t *slotsTable = slotsTableBuild(output.pool, &form->fn.table);

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
      compile(&form->fn.forms.forms[i], fnOutput);
    }

    uint8_t retCode[] = { I_RET };
    codeAppend(fnOutput, sizeof(retCode), retCode);
  }

  FnConstant fnConst;
  constantFnInitContents(&fnConst);

  fnConst.hasName = form->fn.hasName;
  fnConst.name = NULL;
  if (fnConst.hasName) {
    fnConst.name = copyText(output.pool, form->fn.name.value, form->fn.name.length);
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
    table->fileName = copyText(output.pool, output.fileName.value, output.fileName.length);
    table->numLineNumbers = lineNumbers.numUsed;
    table->lineNumbers = lineNumbers.numbers;
  }

  Constant c;
  c.type = CT_FN;
  c.function = fnConst;
  constantAppend(output, c);
}


void compileFn(Form *form, Output output) {
  compileFnConstant(form, output);
  uint16_t fnConstIndex = output.constants->numUsed - 1;

  if (form->fn.isClosure) {

    for (uint16_t i=0; i<form->fn.table.usedSpace; i++) {
      Binding *b = &form->fn.table.bindings[i];
      if (b->source == BS_CAPTURED) {
        uint16_t slotIndex = output.slotsTable[b->captured.bindingIndex];
        uint8_t code[] = { I_LOAD_LOCAL, slotIndex >> 8, slotIndex & 0xFF };
        codeAppend(output, sizeof(code), code);
      }
    }

    uint8_t loadInst = I_LOAD_CLOSURE;
    uint8_t code[] = {
        loadInst,
        fnConstIndex >> 8, fnConstIndex & 0xFF,
        form->fn.numCaptures >> 8, form->fn.numCaptures & 0xFF
    };
    codeAppend(output, sizeof(code), code);
  }
  else {
    uint8_t loadInst = I_LOAD_CONST;
    uint8_t code[] = { loadInst, fnConstIndex >> 8, fnConstIndex & 0xFF };
    codeAppend(output, sizeof(code), code);
  }
}

uint16_t nilConstantGetIndex(Output output) {
  // look for already-defined nil ref constant
  for (uint16_t i=0; i<output.constants->numUsed; i++) {
    Constant *c = &output.constants->constants[i];
    if (c->type == CT_NIL) {
      return i;
    }
  }

  // create nil constant
  Constant c;
  c.type = CT_NIL;
  constantAppend(output, c);
  return output.constants->numUsed - 1;
}

uint16_t uintConstantGetIndex(uint64_t value, Output output) {

  // look for already-defined uint constant
  for (uint16_t i=0; i<output.constants->numUsed; i++) {
    Constant *c = &output.constants->constants[i];
    if (c->type == CT_INT && c->integer == value) {
      return i;
      return R_SUCCESS;
    }
  }

  // create int constant
  Constant c;
  c.type = CT_INT;
  c.integer = value;
  constantAppend(output, c);
  return output.constants->numUsed - 1;
}

uint16_t varRefConstantGetIndex(Text name, Output output) {

  // look for already-defined var ref constant
  for (uint16_t i=0; i<output.constants->numUsed; i++) {
    Constant *c = &output.constants->constants[i];
    if (c->type == CT_SYMBOL && wcscmp(c->symbol.value, name.value) == 0) {
      return i;
    }
  }

  // create or reuse var ref constant
  Constant c;
  c.type = CT_SYMBOL;
  c.symbol.length = name.length;
  c.symbol.value = copyText(output.pool, name.value, c.symbol.length);
  constantAppend(output, c);
  return output.constants->numUsed - 1;
}

void appendMeta(Output output, Form *constant, ConstantMeta *meta) {
  constantMetaInit(meta);
  if (constant->source.isSet) {

    if (constant->source.lineNumber == 0) {
      explode("line number is required if source has been set");
    }

    meta->numProperties = 1;
    palloc(output.pool, meta->properties, sizeof(ConstantMetaProperty) * meta->numProperties,
        "ConstantMetaProperty array");
    ConstantMetaProperty *lineNo = &meta->properties[0];

    {
      wchar_t *keyName = L"line-number";
      Constant key;
      key.type = CT_KEYWORD;
      key.keyword.length = wcslen(keyName);
      key.keyword.value = copyText(output.pool, keyName, key.keyword.length);
      constantAppend(output, key);
      uint16_t index = output.constants->numUsed - 1;
      lineNo->keyIndex = index;
    }

    lineNo->valueIndex = uintConstantGetIndex(constant->source.lineNumber, output);
  }
}

void constInitContents(Form *constant, Constant *c, Output output) {
  switch (constant->type) {

    case F_NUMBER: {
      c->type = CT_INT;
      c->integer = constant->number.value;
      break;
    }
    case F_CHAR: {
      c->type = CT_CHAR;
      c->chr = constant->chr.value;
      break;
    }
    case F_NIL: {
      c->type = CT_NIL;
      break;
    }
    case F_BOOLEAN: {
      c->type = CT_BOOL;
      c->boolean = (uint8_t) constant->boolean.value;
      break;
    }
    case F_STRING: {
      c->type = CT_STR;
      c->string.length = constant->string.length;
      c->string.value = copyText(output.pool, constant->string.value, c->string.length);
      break;
    }
    case F_SYMBOL: {
      c->type = CT_SYMBOL;
      c->symbol.length = constant->symbol.length;
      c->symbol.value = copyText(output.pool, constant->symbol.value, c->symbol.length);
      break;
    }
    case F_KEYWORD: {
      c->type = CT_KEYWORD;
      c->keyword.length = constant->keyword.length;
      c->keyword.value = copyText(output.pool, constant->keyword.value, c->keyword.length);
      break;
    }
    default:
      explode("unsupported: %u", constant->type);
  }
}

void compileConst(Form *form, Output output) {
  Constant c;
  constInitContents(form, &c, output);
  constantAppend(output, c);

  uint16_t index = output.constants->numUsed - 1;
  uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
  codeAppend(output, sizeof(code), code);
}

void compileDef(Form *form, Output output) {
  if (form->def.value == NULL) {
    uint16_t index = nilConstantGetIndex(output);
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    codeAppend(output, sizeof(code), code);
  }
  else {
    compile(form->def.value, output);
  }

  uint16_t index = varRefConstantGetIndex(form->def.name, output);

  uint8_t code[] = { I_DEF_VAR, index >> 8, index & 0xFF };
  codeAppend(output, sizeof(code), code);
}

void compileVarRef(Form *form, Output output) {
  uint16_t index = varRefConstantGetIndex(form->varRef.name, output);
  uint8_t code[] = { I_LOAD_VAR, index >> 8, index & 0xFF };
  codeAppend(output, sizeof(code), code);
}

void compileLet(Form *form, Output output) {
  // 'let's add locals to the current frame

  for (uint16_t i=0; i<form->let.numBindings; i++) {
    LetBinding *binding = &form->let.bindings[i];

    compile(binding->value, output);

    uint16_t index = output.slotsTable[binding->bindingIndex];
    uint8_t code[] = { I_STORE_LOCAL, index >> 8, index & 0xFF };
    codeAppend(output, sizeof(code), code);
  }

  if (form->let.forms.numForms > 0) {
    for (uint16_t i = 0; i < form->let.forms.numForms; i++) {
      Form *f = &form->let.forms.forms[i];
      compile(f, output);
    }
  }
  else {
    Constant c;
    c.type = CT_NIL;
    constantAppend(output, c);

    uint16_t index = output.constants->numUsed - 1;
    uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
    codeAppend(output, sizeof(code), code);
  }
}

void compileEnvRef(Form *form, Output output) {
  uint16_t index = output.slotsTable[form->envRef.bindingIndex];
  uint8_t code[] = { I_LOAD_LOCAL, index >> 8, index & 0xFF };
  codeAppend(output, sizeof(code), code);
}

void compileBuiltin(Form *form, Output output) {
  FormBuiltin *builtin = &form->builtin;

  if (wcscmp(builtin->name.value, L"add") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      compile(&form->builtin.args.forms[i], output);
    }

    uint8_t addCode[] = { I_ADD};
    codeAppend(output, sizeof(addCode), addCode);
  }
  else if (wcscmp(builtin->name.value, L"subtract") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      compile(&form->builtin.args.forms[i], output);
    }

    uint8_t addCode[] = { I_SUB };
    codeAppend(output, sizeof(addCode), addCode);
  }
  else if (wcscmp(builtin->name.value, L"compare") == 0) {

    for (int i=0; i < form->builtin.args.numForms; i++) {
      compile(&form->builtin.args.forms[i], output);
    }

    uint8_t addCode[] = { I_CMP };
    codeAppend(output, sizeof(addCode), addCode);
  }
  else {
    explode("unsupported builtin '%ls'", builtin->name.value);
  }
}

void compileFnCall(Form *form, Output output) {
  // push the arguments in evaluation (left-to-right) order
  for (uint16_t i = 0; i < form->fnCall.args.numForms; i++) {
    compile(&form->fnCall.args.forms[i], output);
  }

  if (form->fnCall.tailPosition && form->fnCall.recurses) {
    uint16_t numForms = form->fnCall.args.numForms;
    uint8_t code[] = {I_INVOKE_DYN_TAIL_RECURSE, numForms >> 8, numForms & 0xFF};
    codeAppend(output, sizeof(code), code);
  }
  else {

    // push the callable
    compile(form->fnCall.fnCallable, output);

    // invoke

    uint8_t inst;
    if (form->fnCall.tailPosition) {
      inst = I_INVOKE_DYN_TAIL;
    } else {
      inst = I_INVOKE_DYN;
    }
    uint16_t numForms = form->fnCall.args.numForms;
    uint8_t code[] = {inst, numForms >> 8, numForms & 0xFF};
    codeAppend(output, sizeof(code), code);
  }
}

void compileList(Form *form, Output output) {
  uint64_t numForms = form->list.length;

  if (numForms == 0) {
    {
      Constant c;
      c.type = CT_NIL;
      constantAppend(output, c);
      uint16_t index = output.constants->numUsed - 1;
      uint8_t code[] = {I_LOAD_CONST, index >> 8, index & 0xFF};
      codeAppend(output, sizeof(code), code);
    }
  }
  else {

    Form *forms;
    {
      palloc(output.pool, forms, sizeof(Form) * numForms, "Constant array");
      ListElement *listElem = form->list.head;
      for (int i = 0; i < numForms; i++) {
        uint16_t idx = numForms - (i + 1);
        forms[idx] = *listElem->expr;
        listElem = listElem->next;
      }
    }

    for (int i=0; i < numForms; i++) {
      Form *f = &forms[i];
      if (i == 0) {
        compile(f, output);

        Constant c;
        c.type = CT_NIL;
        constantAppend(output, c);
        uint16_t index = output.constants->numUsed - 1;
        uint8_t code[] = {I_LOAD_CONST, index >> 8, index & 0xFF};
        codeAppend(output, sizeof(code), code);

        uint8_t addCode[] = {I_CONS};
        codeAppend(output, sizeof(addCode), addCode);
      }
      else {
        compile(f, output);
        uint8_t addCode[] = {I_SWAP, I_CONS};
        codeAppend(output, sizeof(addCode), addCode);
      }

    }
  }
}

void compileVec(Form *form, Output output) {
  Text varName;
  varName.value = L"vector";
  varName.length = wcslen(varName.value);

  Form fnCallable;
  formInitContents(&fnCallable);
  fnCallable.type = F_VAR_REF;
  fnCallable.varRef.name = varName;

  Forms args;
  formsInitContents(&args);
  args.numForms = form->vec.length;
  palloc(output.pool, args.forms, sizeof(Form) * args.numForms, "Form array");

  ListElement *elem = form->vec.head;
  for (uint64_t i=0; i<args.numForms; i++) {
    args.forms[i] = *elem->expr;
    elem = elem->next;
  }

  Form fnCall;
  formInitContents(&fnCall);
  fnCall.type = F_FN_CALL;
  fnCallInitContents(&fnCall.fnCall);
  fnCall.fnCall.fnCallable = &fnCallable;
  fnCall.fnCall.args = args;

  compileFnCall(&fnCall, output);
}

void compileMap(Form *form, Output output) {
  Text varName;
  varName.value = L"hash-map";
  varName.length = wcslen(varName.value);

  Form fnCallable;
  formInitContents(&fnCallable);
  fnCallable.type = F_VAR_REF;
  fnCallable.varRef.name = varName;

  Forms args;
  formsInitContents(&args);
  args.numForms = form->map.length * 2;
  palloc(output.pool, args.forms, sizeof(Form) * args.numForms, "Form array");

  MapElement *elem = form->map.head;
  for (uint64_t i=0; i<args.numForms; i+=2) {
    args.forms[i] = *elem->key;
    args.forms[i+1] = *elem->value;
    elem = elem->next;
  }

  Form fnCall;
  formInitContents(&fnCall);
  fnCall.type = F_FN_CALL;
  fnCallInitContents(&fnCall.fnCall);
  fnCall.fnCall.fnCallable = &fnCallable;
  fnCall.fnCall.args = args;

  compileFnCall(&fnCall, output);
}

void compileHandler(Form *form, Output output) {
  compile(form->handler.handler, output);

  // emit the pushHandler code, keep a pointer to the jump address
  uint16_t jumpAddrOffset;
  {
    uint8_t code[] = { I_PUSH_HANDLER, 0, 0 };
    codeAppend(output, sizeof(code), code);
    jumpAddrOffset = output.codes->numUsed - 2;
  }

  // emit all the forms
  Forms forms = form->handler.forms;
  if (forms.numForms == 0) {
    {
      Constant c;
      c.type = CT_NIL;
      constantAppend(output, c);
      uint16_t index = output.constants->numUsed - 1;
      uint8_t code[] = {I_LOAD_CONST, index >> 8, index & 0xFF};
      codeAppend(output, sizeof(code), code);
    }
  }
  else {
    for (int i=0; i < forms.numForms; i++) {
      Form *f = &form->handler.forms.forms[i];
      compile(f, output);
    }
  }

  // the jumpAddrOffset should point to the first address after the forms
  {
    uint16_t nextCodeAddr = output.codes->numUsed;
    setIndexAtOffset(output, jumpAddrOffset, nextCodeAddr);
  }

  // remove the handler
  uint8_t code[] = { I_POP_HANDLER };
  codeAppend(output, sizeof(code), code);
}

void compile(Form *form, Output output) {
  sourceAppend(form, output);

  switch (form->type) {

    case F_NUMBER:
    case F_CHAR:
    case F_NIL:
    case F_BOOLEAN:
    case F_STRING:
    case F_SYMBOL:
    case F_KEYWORD:
      compileConst(form, output);
      break;

    case F_IF:
      compileIf(form, output);
      break;

    case F_DEF:
      compileDef(form, output);
      break;

    case F_VAR_REF:
      compileVarRef(form, output);
      break;

    case F_LET:
      compileLet(form, output);
      break;

    case F_ENV_REF:
      compileEnvRef(form, output);
      break;

    case F_BUILTIN:
      compileBuiltin(form, output);
      break;

    case F_FN:
      compileFn(form, output);
      break;

    case F_FN_CALL:
      compileFnCall(form, output);
      break;

    case F_LIST:
      compileList(form, output);
      break;

    case F_VEC:
      compileVec(form, output);
      break;

    case F_MAP:
      compileMap(form, output);
      break;

    case F_HANDLER:
      compileHandler(form, output);
      break;

    default:
      explode("unsupported");
  }
}

void compileTopLevel(Pool_t pool, FormRoot *root, CodeUnit *codeUnit) {
  Constants constants;
  Codes codes;
  uint16_t *slotsTable;
  LineNumbers lineNumbers;

  constantsInitContents(&constants);
  codesInitContents(&codes);
  codeUnitInitContents(codeUnit);
  lineNumbersInitContents(&lineNumbers);

  slotsTable = slotsTableBuild(pool, &root->table);

  {
    Output output;
    output.pool = pool;
    output.constants = &constants;
    output.codes = &codes;
    output.slotsTable = slotsTable;
    output.lineNumbers = &lineNumbers;
    output.hasFileName = root->hasFileName;
    output.fileName = root->fileName;

    compile(root->form, output);

    uint8_t code[] = { I_RET };
    codeAppend(output, sizeof(code), code);
  }

  codeUnit->code.numLocals = root->table.usedSpace;
  codeUnit->numConstants = constants.numUsed;
  codeUnit->constants = constants.constants;
  codeUnit->code.codeLength = codes.numUsed;
  codeUnit->code.code = codes.codes;

  if (root->hasFileName) {
    codeUnit->code.hasSourceTable = true;

    SourceTable *table = &codeUnit->code.sourceTable;
    sourceTableInitContents(table);

    table->fileName = copyText(pool, root->fileName.value, root->fileName.length);
    table->numLineNumbers = lineNumbers.numUsed;
    table->lineNumbers = lineNumbers.numbers;
  }

  codeUnit->code.maxOperandStackSize = computeOpStackSize(codeUnit->code.code, codeUnit->code.codeLength);
}



