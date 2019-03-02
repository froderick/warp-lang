#include "repl.h"
#include <stdio.h>

int main(void) {
  RetVal ret;

  char *stdLib = "/Users/ddcmhenry/dev/funtastic/branches/warp-lang/core.lsp";

  Error error;
  VM_t vm;
  InputStream_t source;
  TokenStream_t stream;

  errorInitContents(&error);

  throws(tryVMMake(&vm, &error));
  throws(tryLoad(vm, stdLib, &error));

  throws(tryFileInputStreamMake(stdin, &source, &error));
  throws(tryStreamMake(source, &stream, &error));

  FileInfo fileInfo;
  fileInfoInitContents(&fileInfo);

  while (1) {

    CodeUnit unit;

    errorInitContents(&error);
    codeUnitInitContents(&unit);

    ret = tryReplCompile(stream, fileInfo, vm, &unit, &error);
    if (ret == R_EOF) {
      break;
    }

    if (ret != R_SUCCESS) {
      printf("> encountered compiler error\n\n");
      continue;
    }

    printCodeUnit(&unit);

    VMEvalResult result;
    ret = tryVMEval(vm, &unit, &result, &error);

    if (ret != R_SUCCESS) {
      printf("> encountered eval error\n\n");
      continue;
    }

    if (result.type == RT_RESULT) {
      printf("> ");
      throws(tryExprPrn(&result.result, &error));
      printf("\n");
    }
    else if (result.type == RT_EXCEPTION) {
      printf("> encountered exception:\n\n");
      throws(tryExceptionPrintf(&result.exception, &error));
    }
    else {
      printf("> encountered unhandled eval result type\n\n");
    }

    codeUnitFreeContents(&unit);
  }

  return R_SUCCESS;

  failure:
    printf("encountered terminal errors\n");
    return -1;
}


