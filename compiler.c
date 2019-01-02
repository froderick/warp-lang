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

RetVal tryCompile(Form *form, Constants *constants, Codes *codes, Error *error);

RetVal tryCompileIf(Form *form, Constants *constants, Codes *codes, Error *error) {
  RetVal ret;

  Codes ifBranch, elseBranch;

  codesInitContents(&ifBranch);
  codesInitContents(&elseBranch);

  // emit the test in the main code space
  throws(tryCompile(form->iff.test, constants, codes, error));

  // emit ifBranch form in temporary code space
  throws(tryCompile(form->iff.ifBranch, constants, &ifBranch, error));

  if (form->iff.elseBranch == NULL) {

    // emit test
    // emit I_JMP_IF_NOT $END_ADDR
    // emit ifBranch {
    //  - do stuff
    // }
    // $END_ADDR

    // compute the conditional code, lengths and offsets
    uint16_t nextCodeAddr = codes->numUsed;
    uint16_t ifBranchLength = ifBranch.numUsed;
    uint16_t jumpToEndAddr = nextCodeAddr + ifBranchLength;
    uint8_t jumpToEndCode[] = { I_JMP_IF_NOT, jumpToEndAddr >> 8, jumpToEndAddr & 0xFF };

    // emit the code for real
    throws(tryCodeAppend(codes, sizeof(jumpToEndCode), jumpToEndCode, error));
    throws(tryCodeAppendContents(&ifBranch, codes, error));
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
    throws(tryCompile(form->iff.elseBranch, constants, &elseBranch, error));

    // compute the conditional code, lengths and offsets
    uint16_t nextCodeAddr = codes->numUsed;
    uint16_t jumpToElseCodeLength = 3;
    uint16_t ifBranchLength = ifBranch.numUsed;
    uint16_t jumpToEndCodeLength = 3;
    uint16_t elseBranchLength = elseBranch.numUsed;

    uint16_t jumpToElseAddr = nextCodeAddr + jumpToElseCodeLength + ifBranchLength + jumpToEndCodeLength;
    uint8_t jumpToElseCode[] = {I_JMP_IF_NOT, jumpToElseAddr >> 8, jumpToElseAddr & 0xFF};

    uint16_t jumpToEndAddr = jumpToElseAddr + elseBranchLength;
    uint8_t jumpToEndCode[] = {I_JMP, jumpToEndAddr >> 8, jumpToEndAddr & 0xFF};

    // emit the code for real
    throws(tryCodeAppend(codes, sizeof(jumpToElseCode), jumpToElseCode, error));
    throws(tryCodeAppendContents(&ifBranch, codes, error));
    throws(tryCodeAppend(codes, sizeof(jumpToEndCode), jumpToEndCode, error));
    throws(tryCodeAppendContents(&elseBranch, codes, error));
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


RetVal tryCompileFn(Form *form, FnConstant *fnConstant, Error *error) {
  RetVal ret;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryCompile(Form *form, Constants *constants, Codes *codes, Error *error) {
  RetVal ret;

  switch (form->type) {

    case F_CONST: {

      if (form->constant->type != N_NUMBER) {
        throwRuntimeError(error, "unsupported");
      }

      Constant c;
      c.type = CT_INT;
      c.integer = form->constant->number.value;
      throws(tryAppendConstant(constants, c, error));

      // emit code
      // but what for? we don't know what should happen to this constant
      // we could just assume that we put it on the opstack

      uint16_t index = constants->numUsed - 1;
      uint8_t code[] = { I_LOAD_CONST, index >> 8, index & 0xFF };

      throws(tryCodeAppend(codes, sizeof(code), code, error));

      break;
    }
    case F_IF: {
      throws(tryCompileIf(form, constants, codes, error));
      break;
    }

    case F_NONE:
    case F_LET:
    case F_DEF:
    case F_ENV_REF:
    case F_VAR_REF:
    case F_FN: {
      throwRuntimeError(error, "unsupported");
      break;
    }

    case F_BUILTIN: {
      FormBuiltin *builtin = &form->builtin;

      if (wcscmp(builtin->name, L"add") == 0) {
        form->type = F_BUILTIN;

        for (int i=0; i < form->builtin.numArgs; i++) {
          throws(tryCompile(&form->builtin.args[i], constants, codes, error));
        }

        uint8_t addCode[] = { I_ADD};
        throws(tryCodeAppend(codes, sizeof(addCode), addCode, error));
      }
      else {
        throwRuntimeError(error, "unsupported fnCallable");
      }

      break;
    }

    case F_FN_CALL: {
      throwRuntimeError(error, "unsupported fnCallable");
      break;
    }
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

  throws(tryCompile(form, &constants, &codes, error));

  codeUnit->numConstants = constants.numUsed;
  codeUnit->constants = constants.constants;
  codeUnit->code.codeLength = codes.numUsed;
  codeUnit->code.code = codes.codes;

  // TODO: we don't populate these yet
  codeUnit->code.numLocals = 0;
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
