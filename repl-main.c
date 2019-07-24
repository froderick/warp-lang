#include "repl.h"
#include "print.h"
#include <stdio.h>

#define ONE_MB (1024 * 1000)

int main(void) {
  RetVal ret;

  //char *stdLib = "/Users/ddcmhenry/dev/funtastic/branches/warp-lang/core.lsp";
//  char *stdLib = "/warp/core.lsp";

  Error error;
  VMConfig config;
  VM_t vm;
  InputStream_t source;
  TokenStream_t stream;
  Pool_t sessionPool = NULL;

  errorInitContents(&error);

  throws(tryPoolCreate(&sessionPool, ONE_MB, &error));

  vmConfigInitContents(&config);
  vm = vmMake(config);
  throws(tryLoad(vm, STD_LIB, &error));

  throws(tryFileInputStreamMake(sessionPool, stdin, &source, &error));
  throws(tryStreamMake(sessionPool, source, &stream, &error));

  FileInfo fileInfo;
  fileInfoInitContents(&fileInfo);

  Pool_t evalPool = NULL;
  throws(tryPoolCreate(&evalPool, ONE_MB, &error));

  while (1) {

    CodeUnit unit;

    errorInitContents(&error);
    codeUnitInitContents(&unit);

    ret = tryReplCompile(evalPool, stream, fileInfo, vm, &unit, &error);
    if (ret == R_EOF) {
      break;
    }

    if (ret != R_SUCCESS) {
      printf("> encountered compiler error\n\n");
      continue;
    }

    printCodeUnit(&unit);

    VMEvalResult result = vmEval(vm, &unit);

    if (result.type == RT_RESULT) {
      printf("> ");
      print(vm, result.value);
      printf("\n");
    }
    else if (result.type == RT_EXCEPTION) {
      printf("> encountered exception:\n\n");
      printException(vm, result.value);
    }
    else {
      printf("> encountered unhandled eval result type\n\n");
    }

    poolClear(evalPool);
  }

  poolClear(sessionPool);

  return R_SUCCESS;

  failure:
    printf("encountered terminal errors\n");
    return -1;
}


