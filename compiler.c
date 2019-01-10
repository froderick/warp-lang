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
  uint16_t *numLocals;
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

RetVal tryCompileFn(Form *form, Output output, Error *error) {
  RetVal ret;

  // clean up on failure
  Constants fnConstants;
  Codes fnCodes;

  constantsInitContents(&fnConstants);
  codesInitContents(&fnCodes);

  uint16_t fnNumLocals = form->fn.numArgs; // the first n locals are always the first n args

  {
    Output fnOutput;
    fnOutput.constants = &fnConstants;
    fnOutput.codes = &fnCodes;
    fnOutput.numLocals = &fnNumLocals;

    for (uint16_t i = 0; i < form->fn.numForms; i++) {
      throws(tryCompile(&form->fn.forms[i], fnOutput, error));
    }
  }

  uint8_t retCode[] = { I_RET };
  throws(tryCodeAppend(&fnCodes, sizeof(retCode), retCode, error));

  FnConstant fnConst;
  constantFnInitContents(&fnConst);

  fnConst.numArgs = form->fn.numArgs;
  fnConst.numConstants = fnConstants.numUsed;
  fnConst.constants = fnConstants.constants;
  fnConst.code.numLocals = fnNumLocals;
  fnConst.code.maxOperandStackSize = 100; // TODO: need to compute this
  fnConst.code.codeLength = fnCodes.numUsed;
  fnConst.code.code = fnCodes.codes;
  fnConst.code.hasSourceTable = false;

  Constant c;
  c.type = CT_FN;
  c.function = fnConst;
  throws(tryAppendConstant(output.constants, c, error));

  uint16_t index = output.constants->numUsed - 1;
  uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    constantsFreeContents(&fnConstants);
    codesFreeContents(&fnCodes);
    return ret;
}

typedef struct Text {
  uint64_t length;
  wchar_t *value;
} Text;

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

RetVal tryCompileConst(Form *form, Output output, Error *error) {
  RetVal ret;

  if (form->constant->type != N_NUMBER) {
    throwCompilerError(error, "unsupported");
  }

  Constant c;
  c.type = CT_INT;
  c.integer = form->constant->number.value;
  throws(tryAppendConstant(output.constants, c, error));

  uint16_t index = output.constants->numUsed - 1;
  uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };

  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileDef(Form *form, Output output, Error *error) {
  RetVal ret;

  throws(tryCompile(form->def.value, output, error));

  Text name;
  name.length = form->def.nameLength;
  name.value = form->def.name;

  uint16_t index;
  throws(varRefConstantGetIndex(name, output, &index, error));

  uint8_t code[] = { I_DEF_VAR, index >> 8, index & 0xFF };
  throws(tryCodeAppend(output.codes, sizeof(code), code, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileVarRef(Form *form, Output output, Error *error) {
  RetVal ret;

  Text name;
  name.length = form->varRef.nameLength;
  name.value = form->varRef.name;

  uint16_t index;
  throws(varRefConstantGetIndex(name, output, &index, error));

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
    LexicalBinding *binding = &form->let.bindings[i];

    throws(tryCompile(binding->value, output, error));

    uint16_t index = binding->index;
    uint8_t code[] = { I_STORE_LOCAL, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output.codes, sizeof(code), code, error));

    *output.numLocals = *output.numLocals + 1;
  }

  for (uint16_t i = 0; i<form->let.numForms; i++) {
    Form *f = &form->let.forms[i];
    throws(tryCompile(f, output, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileEnvRef(Form *form, Output output, Error *error) {
  RetVal ret;

  if (form->envRef.type == RT_ARG || form->envRef.type == RT_LOCAL) {
    uint16_t index = form->envRef.index;
    uint8_t code[] = { I_LOAD_LOCAL, index >> 8, index & 0xFF };
    throws(tryCodeAppend(output.codes, sizeof(code), code, error));
  }
  else {
    throwCompilerError(error, "unsupported");
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileBuiltin(Form *form, Output output, Error *error) {
  RetVal ret;

  FormBuiltin *builtin = &form->builtin;

  if (wcscmp(builtin->name, L"add") == 0) {
    form->type = F_BUILTIN;

    for (int i=0; i < form->builtin.numArgs; i++) {
      throws(tryCompile(&form->builtin.args[i], output, error));
    }

    uint8_t addCode[] = { I_ADD};
    throws(tryCodeAppend(output.codes, sizeof(addCode), addCode, error));
  }
  else {
    throwCompilerError(error, "unsupported builtin '%ls'", builtin->name);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompileFnCall(Form *form, Output output, Error *error) {
  RetVal ret;

  // push the arguments in evaluation (left-to-right) order
  for (uint16_t i = 0; i<form->fnCall.numArgs; i++) {
    throws(tryCompile(&form->fnCall.args[i], output, error));
  }

  // push the callable
  throws(tryCompile(form->fnCall.fnCallable, output, error));

  // invoke
  uint8_t code[] = { I_INVOKE_DYN };
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

RetVal tryCompileTopLevel(Form *form, CodeUnit *codeUnit, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Constants constants;
  Codes codes;

  constantsInitContents(&constants);
  codesInitContents(&codes);
  codeUnitInitContents(codeUnit);

  // TODO: this was where I left off, the tryCompile needs to provide more information to fill out the Code object

  uint16_t numLocals = 0;

  {
    Output output;
    output.constants = &constants;
    output.codes = &codes;
    output.numLocals = &numLocals;

    throws(tryCompile(form, output, error));
  }

  codeUnit->code.numLocals = numLocals;
  codeUnit->numConstants = constants.numUsed;
  codeUnit->constants = constants.constants;
  codeUnit->code.codeLength = codes.numUsed;
  codeUnit->code.code = codes.codes;

  // TODO: we don't populate these yet
  codeUnit->code.maxOperandStackSize = 10;
  codeUnit->code.hasSourceTable = false;

  return R_SUCCESS;

  failure:
    constantsFreeContents(&constants);
    codesFreeContents(&codes);
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
