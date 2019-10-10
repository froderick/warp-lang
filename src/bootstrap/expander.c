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

bool isMacro(Expander_t expander, wchar_t *sym) {
  Symbol *symbol = deref(expander->vm, getSymbol(expander->vm, sym));
  return symbol->valueDefined && symbol->isMacro;
}

RetVal tryExpand(Expander *expander, Text sym, Form *input, Form **output, Error *error) {
  RetVal ret;

  if (input->type != F_LIST) {
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
    args.forms[i] = *elem->expr;
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

  compileTopLevel(expander->pool, &root, &codeUnit);

  VMEvalResult result = vmEval(expander->vm, &codeUnit);

  if (result.type == RT_RESULT) {

    Form *expr = printToReader(expander->vm, expander->pool, result.value);

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
