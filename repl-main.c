#include "repl.h"
#include <stdio.h>

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
    printCodeUnit(&unit);

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





































