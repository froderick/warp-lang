#include "repl.h"
#include <stdio.h>

int main(void) {
  RetVal ret;

  char *stdLib = "/Users/ehenry/dev/funtastic/branches/warp-lang/core.lsp";

  Error error;
  VM_t vm;
  InputStream_t source;
  TokenStream_t stream;

  errorInitContents(&error);

  throws(tryVMMake(&vm, &error));
  throws(tryLoad(vm, stdLib, &error));

  throws(tryFileInputStreamMake(stdin, &source, &error));
  throws(tryStreamMake(source, &stream, &error));

  while (1) {

    CodeUnit unit;

    errorInitContents(&error);
    codeUnitInitContents(&unit);

    ret = tryReplCompile(stream, vm, &unit, &error);
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





































