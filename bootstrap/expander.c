#include "../errors.h"
#include "analyzer.h"
#include "compiler.h"
#include "../vm/vm.h"
#include "expander.h"
#include "ast.h"
#include "pool.h"
#include "print.h"

typedef struct Expander {
  Pool_t pool;
  VM_t vm;
} Expander;

RetVal tryMakeExpander(Pool_t pool, Expander **expander, VM_t vm, Error *error) {
  RetVal ret;

  tryPalloc(pool, *expander, sizeof(Expander), "Expander");

  (*expander)->pool = pool;
  (*expander)->vm = vm;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryIsMacro(Expander *expander, Text sym, bool *isMacro, Error *error) {
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
  tryPalloc(expander->pool, args.forms, sizeof(Form) * args.numForms, "Form array");

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

  throws(tryCompileTopLevel(expander->pool, &root, &codeUnit, error));
  output = vmEval(expander->vm, &codeUnit);

  if (output.type == RT_RESULT) {
    if (valueType(output.value) != VT_BOOL) {
      throwInternalError(error, "this should return a boolean");
    }
    *isMacro = unwrapBool(output.value);
    return R_SUCCESS;
  }
  else if (output.type == RT_EXCEPTION) {
    printException(expander->vm, output.value);
    throwInternalError(error, "encountered exception while processing macro: getmacro");
  }
  else {
    throwInternalError(error, "unhandled eval result type");
  }

  failure:
    return ret;
}

RetVal tryExpand(Expander *expander, Text sym, Expr *input, Expr **output, Error *error) {
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
  tryPalloc(expander->pool, args.forms, sizeof(Form) * args.numForms, "Form array");

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

  throws(tryCompileTopLevel(expander->pool, &root, &codeUnit, error));

  VMEvalResult result = vmEval(expander->vm, &codeUnit);

  if (result.type == RT_RESULT) {

    Expr *expr = printToReader(expander->vm, expander->pool, result.value);

    printf("macroexpand occurred {\n    ");
    exprPrn(expander->pool, input);
    printf("\n    =>\n    ");
    exprPrn(expander->pool, expr);
    printf("\n}\n");

    *output = expr;

    return R_SUCCESS;
  }
  else if (result.type == RT_EXCEPTION) {
    printException(expander->vm, result.value);
    throwInternalError(error, "encountered exception while processing macro: %ls", sym.value);
  }
  else {
    throwInternalError(error, "unhandled eval result type");
  }

  return R_SUCCESS;
  failure:
    return ret;

}
