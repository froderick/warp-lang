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
  exprInitContents(&builtin);
  builtin.type = N_SYMBOL;
  builtin.symbol.length = wcslen(builtinSym);
  builtin.symbol.value = builtinSym;

  Expr getmacro;
  exprInitContents(&getmacro);
  getmacro.type = N_KEYWORD;
  getmacro.symbol.length = wcslen(getMacro);
  getmacro.symbol.value = getMacro;

  Expr macro;
  exprInitContents(&macro);
  macro.type = N_STRING;
  macro.string.length = sym.length;
  macro.string.value = sym.value;

  Expr callExpr;
  exprInitContents(&callExpr);
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
  VMEvalResult output;

  throws(tryFormAnalyzeOptions(options, &callExpr, &root, error));
  throws(tryCompileTopLevel(root, &codeUnit, error));
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

  Expr macro;
  exprInitContents(&macro);
  macro.type = N_SYMBOL;
  macro.string.length = sym.length;
  macro.string.value = sym.value;

  Expr quoteSym;
  exprInitContents(&quoteSym);
  quoteSym.type = N_SYMBOL;
  quoteSym.symbol.value = L"quote";
  quoteSym.symbol.length = wcslen(L"quote");

  Expr quote;
  exprInitContents(&quote);
  quote.type = N_LIST;
  listInitContents(&quote.list);
  throws(tryListAppend(&quote.list, &quoteSym, error)); // all macro functions take a single argument, a list of the supplied arguments
  throws(tryListAppend(&quote.list, input, error)); // all macro functions take a single argument, a list of the supplied arguments

  Expr callExpr;
  exprInitContents(&callExpr);
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

  /*
   * TODO: when we invoke eval here, we get back an expanded expression that has no line numbers associated with it at all
   *
   * Find a way for eval to save the known line numbers.
   * - maybe include them somehow as metadata on the exprs such that when the macro references them, the lines are
   *   preserved
   * - or find a way to match up the expanded form of the exprs to the unexpanded forms and add back the missing
   *   line numbers by passing through some kind of expr object reference id.
   *
   * *Either way, this is going to involve a re-work of the expander.*
   *
   * There might be another way. All the line numbers that apply to a given macro-expansion area already included in
   * the code unit for doing the macro-expansion. That means if the VM wanted, it could look at them and reuse them.
   * The trouble is that these line numbers become invalid because they are based on code-offsets, which will likely
   * be changed by the new macro output.
   *
   * Let's think about this some more:
   * - we would only expect to get line numbers in macro output where a macro emits values that directly correspond 1:1
   *   with code the user wrote. This would only happen when the macro defines an fn?
   *
   * What if thi mostly doesn't matter? line numbers are for the code that was written, not the code that was generated.
   * Shouldn't it be fine to pretend that the line numbers are correct?
   *
   * If an expr A is rewritten into expr B, it should still get the source lines for expr A. We dont need new source
   * lines that are B-specific. But, we do need to know what expresssions in B refer to source lines from A. Which are
   * those? How do we get back partial source lines?
   *
   *
   * X expr -> X constant -> value -> macro-fn -> value -> expr
   */

  return R_SUCCESS;

  failure:
    return ret;

}
