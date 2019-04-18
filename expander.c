#include "errors.h"
#include "analyzer.h"
#include "compiler.h"
#include "vm.h"
#include "expander.h"
#include "reader.h"

typedef struct Expander {
  VM_t vm;
} Expander;

RetVal tryMakeExpander(Expander **expander, VM_t vm, Error *error) {
  RetVal ret;

  tryMalloc(*expander, sizeof(Expander), "Expander");

  (*expander)->vm = vm;

  return R_SUCCESS;

  failure:
    return ret;
}

void freeExpander(Expander_t expander) {
  if (expander != NULL) {
    expander->vm = NULL;
    free(expander);
  }
}

RetVal tryIsMacro(Expander_t expander, Text sym, bool *isMacro, Error *error) {
  RetVal ret;

  wchar_t getMacro[] = L"get-macro";

  Form fnCallable;
  formInitContents(&fnCallable);
  fnCallable.type = F_VAR_REF;
  fnCallable.varRef.name.length = wcslen(getMacro);
  fnCallable.varRef.name.value = getMacro;

  Forms args;
  formsInitContents(&args);
  args.numForms = 1;
  tryMalloc(args.forms, sizeof(Form) * args.numForms, "Form array");

  Expr varName;
  exprInitContents(&varName);
  varName.type = N_SYMBOL;
  varName.symbol.length = sym.length;
  varName.symbol.value = sym.value;

  Form *arg = &args.forms[0];
  formInitContents(arg);
  arg->type = F_CONST;
  arg->constant = &varName;

  Form fnCall;
  formInitContents(&fnCall);
  fnCall.type = F_FN_CALL;
  fnCallInitContents(&fnCall.fnCall);
  fnCall.fnCall.fnCallable = &fnCallable;
  fnCall.fnCall.args = args;

  FormRoot root;
  rootInitContents(&root);
  root.form = &fnCall;

  CodeUnit codeUnit;
  VMEvalResult output;

  throws(tryCompileTopLevel(&root, &codeUnit, error));
  throws(tryVMEval(expander->vm, &codeUnit, &output, error));

  if (output.type == RT_RESULT) {
    if (output.result.type != N_BOOLEAN) {
      throwInternalError(error, "this should return a boolean");
    }
    *isMacro = output.result.boolean.value;
    return R_SUCCESS;
  }
  else if (output.type == RT_EXCEPTION) {
    throws(tryExceptionPrintf(&output.exception, error));
    throwInternalError(error, "encountered exception while processing macro: getmacro");
  }
  else {
    throwInternalError(error, "unhandled eval result type");
  }

  failure:
    return ret;
}

RetVal tryExpand(Expander *expander, Text sym, Expr *input, VMEvalResult *output, Error *error) {
  RetVal ret;

  if (input->type != N_LIST) {
    throwInternalError(error, "macro input expr must always be a list: %u", input->type);
  }

  Form fnCallable;
  formInitContents(&fnCallable);
  fnCallable.type = F_VAR_REF;
  fnCallable.varRef.name = sym;

  Forms args;
  formsInitContents(&args);
  args.numForms = input->list.length;
  tryMalloc(args.forms, sizeof(Form) * args.numForms, "Form array");

  ListElement *elem = input->list.head;
  for (uint64_t i=0; i<args.numForms; i++) {
    Form *arg = &args.forms[i];
    formInitContents(arg);
    arg->type = F_CONST;
    arg->constant = elem->expr;
    elem = elem->next;
  }

  Form fnCall;
  formInitContents(&fnCall);
  fnCall.type = F_FN_CALL;
  fnCallInitContents(&fnCall.fnCall);
  fnCall.fnCall.fnCallable = &fnCallable;
  fnCall.fnCall.args = args;

  FormRoot root;
  rootInitContents(&root);
  root.form = &fnCall;

  CodeUnit codeUnit;

  throws(tryCompileTopLevel(&root, &codeUnit, error));
  throws(tryVMEval(expander->vm, &codeUnit, output, error));

  return R_SUCCESS;

  failure:
    return ret;

}
