#include "compiler.h"

RetVal tryReplCompile(TokenStream_t stream, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  Expr *expr = NULL;
  FormRoot *form = NULL;

  throws(tryExprRead(stream, &expr, error));
  throws(tryFormAnalyze(expr, &form, error));
  throws(tryCompileTopLevel(form, codeUnit, error));

  ret = R_SUCCESS;
  goto finally;

  failure:
    goto finally;

  finally:
    if (expr != NULL) {
      exprFree(expr);
    }
    if (form != NULL) {
      rootFree(form);
    }
    return ret;
}

RetVal tryReplEval(wchar_t *inputText, wchar_t **outputText, Error *error) {
  RetVal ret;

  CodeUnit unit;
  InputStream_t source;
  TokenStream_t stream;
  VM_t vm;
  Value result;

  printf("%ls\n", inputText);

  codeUnitInitContents(&unit);

  throws(tryStringInputStreamMake(inputText, wcslen(inputText), &source, error));
  throws(tryStreamMake(source, &stream, error));
  throws(tryVMMake(&vm, error));

  throws(tryReplCompile(stream, &unit, error));
  printCodeUnit(&unit);

  throws(tryVMEval(vm, &unit, &result, error));
  throws(tryVMPrnStr(vm, result, outputText, error));

  tryStreamFree(stream, error);
  vmFreeContents(vm);
  codeUnitFreeContents(&unit);

  return R_SUCCESS;

  failure:
    tryStreamFree(stream, error); // frees input stream also
    vmFreeContents(vm);
    codeUnitFreeContents(&unit);
    return ret;
}






































