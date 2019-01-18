#include "repl.h"

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





































