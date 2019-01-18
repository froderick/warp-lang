#include "compiler.h"

RetVal tryReplCompile(TokenStream_t stream, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  EnvBindingStack bindingStack;
  Expr *expr;
  Form *form;

  envBindingStackInit(&bindingStack);

  throws(tryExprRead(stream, &expr, error));
  throws(tryFormAnalyze(&bindingStack, expr, &form, error));
  throws(tryCompileTopLevel(form, codeUnit, error));

  ret = R_SUCCESS;
  goto finally;

  failure:
    goto finally;

  finally:
    envBindingStackFreeContents(&bindingStack);
    exprFree(expr);
    formFree(form);
    return ret;
}

RetVal tryReplEval(wchar_t *inputText, wchar_t *outputText, Error *error) {
  RetVal ret;

  CodeUnit unit;
  InputStream_t source;
  TokenStream_t stream;
  VM_t vm;
  Value result;

  codeUnitInitContents(&unit);

  throws(tryStringInputStreamMake(inputText, wcslen(inputText), &source, error));
  throws(tryStreamMake(source, &stream, error));

  throws(tryReplCompile(stream, &unit, error));
  throws(tryVMMake(&vm, error));
  throws(tryVMEval(vm, &unit, &result, error));

  throws(tryVMPrn(vm, result, error));

  tryInputStreamFree(source, error);
  vmFreeContents(vm);
  codeUnitFreeContents(&unit);

  return R_SUCCESS;

  failure:
    tryStreamFree(stream, error); // frees input stream also
    vmFreeContents(vm);
    codeUnitFreeContents(&unit);
    return ret;
}

int main(void) {
  RetVal ret;

  Error error;
  InputStream_t source;
  TokenStream_t stream;
  VM_t vm;

  errorInitContents(&error);

  throws(tryFileInputStreamMake(stdin, &source, &error));
  throws(tryStreamMake(source, &stream, &error));
  throws(tryVMMake(&vm, &error));

  while (1) {

    CodeUnit unit;

    errorInitContents(&error);
    codeUnitInitContents(&unit);

    ret = tryReplCompile(stream, &unit, &error);
    if (ret == R_EOF) {
      break;
    }

    if (ret != R_SUCCESS) {
      printf("> encountered compiler error\n\n");
      continue;
    }

    printCodeUnit(&unit);

    Value result;
    ret = tryVMEval(vm, &unit, &result, &error);

    if (ret != R_SUCCESS) {
      printf("> encountered eval error\n\n");
      continue;
    }

    printf("> ");
    throws(tryVMPrn(vm, result, &error));
    printf("\n");

    codeUnitFreeContents(&unit);
  }

  return R_SUCCESS;

  failure:
    printf("encountered terminal errors\n");
    return -1;
}





































