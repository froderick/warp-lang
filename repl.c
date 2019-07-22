#include <string.h>
#include <libgen.h>
#include "repl.h"
#include "compiler.h"

void fileInfoInitContents(FileInfo *f) {
  f->hasFileName = false;
  textInitContents(&f->fileName);
}

#define ONE_MB (1024 * 1000)

RetVal tryReplCompile(Pool_t outputPool, TokenStream_t stream, FileInfo fileInfo, VM_t vm, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  Expr *expr = NULL;
  FormRoot *form = NULL;
  Expander_t expander = NULL;

  throws(tryMakeExpander(outputPool, &expander, vm, error));

  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);
  options.expander = expander;

  if (fileInfo.hasFileName) {
    options.hasFileName = true;
    options.fileName = fileInfo.fileName;
  }

  throws(tryExprRead(outputPool, stream, &expr, error));
  throws(tryFormAnalyzeOptions(options, expr, outputPool, &form, error));
  throws(tryCompileTopLevel(outputPool, form, codeUnit, error));

  ret = R_SUCCESS;
  goto finally;

  failure:
    goto finally;

  finally:
    return ret;
}

RetVal tryReplEvalConf(Pool_t outputPool, wchar_t *inputText, wchar_t **outputText, bool useStdLib, VMConfig config,
    Error *error) {

  RetVal ret;

  CodeUnit unit;
  InputStream_t source = NULL;
  TokenStream_t stream = NULL;
  VM_t vm = NULL;
  Pool_t pool = NULL;
  VMEvalResult result;

  printf("%ls\n", inputText);

  codeUnitInitContents(&unit);

  throws(tryPoolCreate(&pool, ONE_MB, error));
  throws(tryStringInputStreamMake(pool, inputText, wcslen(inputText), &source, error));
  throws(tryStreamMake(pool, source, &stream, error));
  throws(tryVMMake(&vm, config, error));

  if (useStdLib) {
    throws(tryLoad(vm, STD_LIB, error));
  }

  FileInfo fileInfo;
  fileInfoInitContents(&fileInfo);

  throws(tryReplCompile(pool, stream, fileInfo, vm, &unit, error));
  printCodeUnit(&unit);

  throws(tryVMEval(vm, &unit, pool, &result, error));

  if (result.type == RT_RESULT) {
    throws(tryExprPrnStr(outputPool, &result.result, outputText, error));
  }
  else {
    exceptionPrintf(vm);
//    throws(tryExprPrnStr(outputPool, &result.result, outputText, error));
//    throws(tryExceptionPrintf(&result.exception, error));
  }

  ret = R_SUCCESS;
  goto done;

  failure:
  goto done;

  done:
    tryInputStreamFree(source, error);
    vmFreeContents(vm);
    poolFree(pool);
    return ret;
}

RetVal tryReplEval(Pool_t outputPool, wchar_t *inputText, wchar_t **outputText, VMConfig config, Error *error) {
  return tryReplEvalConf(outputPool, inputText, outputText, true, config, error);
}

RetVal tryTextMakeFromChar(char *filename, Text *to, Error *error) {
  RetVal ret;

  textInitContents(to);

  to->length = strlen(filename) + 1;
  tryMalloc(to->value, to->length * sizeof(wchar_t), "wide string");
  swprintf(to->value, to->length, L"%s", filename);

  return R_SUCCESS;
  failure:
  return ret;
}

#define ONE_MB (1024 * 1000)

RetVal tryLoad(VM_t vm, char *filename, Error *error) {
  RetVal ret;

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_MB, error));

  InputStream_t source;
  TokenStream_t stream;
  FileInfo fileInfo;

  throws(tryFileInputStreamMakeFilename(pool, filename, &source, error));
  throws(tryStreamMake(pool, source, &stream, error));

  fileInfoInitContents(&fileInfo);
  char* baseFileName = basename(filename);
  fileInfo.hasFileName = true;
  throws(tryTextMakeFromChar(baseFileName, &fileInfo.fileName, error));

  CodeUnit unit;


  VMEvalResult result;
  while (true) {

    codeUnitInitContents(&unit);

    ret = tryReplCompile(pool, stream, fileInfo, vm, &unit, error);
    if (ret == R_EOF) {
      break;
    }
    else {
      if (ret != R_SUCCESS) {
        goto failure;
      }
    }
    //printCodeUnit(&unit);

    throws(tryVMEval(vm, &unit, pool, &result, error));
  }

  ret = R_SUCCESS;
  goto done;

  failure:
    goto done;

  done:
    poolFree(pool);
    return ret;
}




































