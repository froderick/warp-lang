#include "repl.h"
#include "compiler.h"

RetVal tryReplCompile(TokenStream_t stream, VM_t vm, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  Expr *expr = NULL;
  FormRoot *form = NULL;
  Expander_t expander = NULL;

  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);

  throws(tryMakeExpander(&expander, vm, error));
  options.expander = expander;

  throws(tryExprRead(stream, &expr, error));
  throws(tryFormAnalyzeOptions(options, expr, &form, error));
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

  //printf("%ls\n", inputText);

  codeUnitInitContents(&unit);

  throws(tryStringInputStreamMake(inputText, wcslen(inputText), &source, error));
  throws(tryStreamMake(source, &stream, error));
  throws(tryVMMake(&vm, error));

  char *stdLib = "/Users/ehenry/dev/funtastic/branches/warp-lang/core.lsp";
  throws(tryLoad(vm, stdLib, error));

  throws(tryReplCompile(stream, vm, &unit, error));
  //printCodeUnit(&unit);

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

RetVal tryLoad(VM_t vm, char *filename, Error *error) {
  RetVal ret;

  InputStream_t source;
  TokenStream_t stream;

  throws(tryFileInputStreamMakeFilename(filename, &source, error));
  throws(tryStreamMake(source, &stream, error));

  CodeUnit unit;
  while (true) {

    codeUnitInitContents(&unit);

    ret = tryReplCompile(stream, vm, &unit, error);
    if (ret == R_EOF) {
      break;
    }
    else {
      if (ret != R_SUCCESS) {
        goto failure;
      }
    }
    //printCodeUnit(&unit);

    Value result;
    throws(tryVMEval(vm, &unit, &result, error));

    throws(tryVMPrn(vm, result, error));

    codeUnitFreeContents(&unit);
  }

  return R_SUCCESS;

  failure:
  codeUnitFreeContents(&unit);
  return ret;
}




































