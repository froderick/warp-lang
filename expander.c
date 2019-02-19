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

  wchar_t builtinSym[] = L"builtin";
  wchar_t getMacro[] = L"getmacro";

  Expr builtin;
  builtin.type = N_SYMBOL;
  builtin.symbol.length = wcslen(builtinSym);
  builtin.symbol.value = builtinSym;

  Expr getmacro;
  getmacro.type = N_KEYWORD;
  getmacro.symbol.length = wcslen(getMacro);
  getmacro.symbol.value = getMacro;

  Expr macro;
  macro.type = N_STRING;
  macro.string.length = sym.length;
  macro.string.value = sym.value;

  Expr callExpr;
  callExpr.type = N_LIST;
  listInitContents(&callExpr.list);

  throws(tryListAppend(&callExpr.list, &builtin, error));
  throws(tryListAppend(&callExpr.list, &getmacro, error));
  throws(tryListAppend(&callExpr.list, &macro, error));

  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);
  options.expander = NULL; // prevents expansion

  FormRoot *root = NULL;
  CodeUnit codeUnit;
  Expr output;

  throws(tryFormAnalyzeOptions(options, &callExpr, &root, error));
  throws(tryCompileTopLevel(root, &codeUnit, error));
  throws(tryVMEval(expander->vm, &codeUnit, &output, error));

  if (output.type != N_BOOLEAN) {
    throwInternalError(error, "this should return a boolean");
  }

  *isMacro = output.boolean.value;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryExpand(Expander *expander, Text sym, Expr *input, Expr *output, Error *error) {
  RetVal ret;

  if (input->type != N_LIST) {
    throwInternalError(error, "macro input expr must always be a list: %u", input->type);
  }

  Expr macro;
  macro.type = N_SYMBOL;
  macro.string.length = sym.length;
  macro.string.value = sym.value;

  Expr quoteSym;
  quoteSym.type = N_SYMBOL;
  quoteSym.symbol.value = L"quote";
  quoteSym.symbol.length = wcslen(L"quote");

  Expr quote;
  quote.type = N_LIST;
  listInitContents(&quote.list);
  throws(tryListAppend(&quote.list, &quoteSym, error)); // all macro functions take a single argument, a list of the supplied arguments
  throws(tryListAppend(&quote.list, input, error)); // all macro functions take a single argument, a list of the supplied arguments

  Expr callExpr;
  callExpr.type = N_LIST;
  listInitContents(&callExpr.list);

  throws(tryListAppend(&callExpr.list, &macro, error));
  throws(tryListAppend(&callExpr.list, &quote, error)); // all macro functions take a single argument, a list of the supplied arguments

  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);
  options.expander = NULL; // prevents expansion

  FormRoot *root = NULL;
  CodeUnit codeUnit;

  throws(tryFormAnalyzeOptions(options, &callExpr, &root, error));
  throws(tryCompileTopLevel(root, &codeUnit, error));
  throws(tryVMEval(expander->vm, &codeUnit, output, error));

  return R_SUCCESS;

  failure:
    return ret;

}
