  #include <string.h>
#include <libgen.h>
#include "repl.h"
#include "compiler.h"

void fileInfoInitContents(FileInfo *f) {
  f->hasFileName = false;
  textInitContents(&f->fileName);
}

void fileInfoFreeContents(FileInfo *f) {
  f->hasFileName = false;
  textFreeContents(&f->fileName);
}

#define ONE_MB (1024 * 1000)

RetVal tryReplCompile(TokenStream_t stream, FileInfo fileInfo, VM_t vm, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  Pool_t pool = NULL;
  Expr *expr = NULL;
  FormRoot *form = NULL;
  Expander_t expander = NULL;

  throws(tryPoolCreate(&pool, ONE_MB, error));

  throws(tryMakeExpander(pool, &expander, vm, error));

  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);
  options.expander = expander;

  if (fileInfo.hasFileName) {
    options.hasFileName = true;
    options.fileName = fileInfo.fileName;
  }

  throws(tryExprRead(pool, stream, &expr, error));
  throws(tryFormAnalyzeOptions(options, expr, pool, &form, error));
  throws(tryCompileTopLevel(form, codeUnit, error));

  ret = R_SUCCESS;
  goto finally;

  failure:
    goto finally;

  finally:
    poolFree(pool);
    return ret;
}

RetVal tryReplEvalConf(wchar_t *inputText, wchar_t **outputText, bool useStdLib, Error *error) {

  RetVal ret;

  CodeUnit unit;
  InputStream_t source = NULL;
  TokenStream_t stream = NULL;
  VM_t vm = NULL;
  Pool_t pool = NULL;
  VMEvalResult result;

  printf("%ls\n", inputText);

  codeUnitInitContents(&unit);

  throws(tryStringInputStreamMake(inputText, wcslen(inputText), &source, error));
  throws(tryStreamMake(source, &stream, error));
  throws(tryVMMake(&vm, error));
  throws(tryPoolCreate(&pool, ONE_MB, error));

  if (useStdLib) {
    throws(tryLoad(vm, STD_LIB, error));
  }

  FileInfo fileInfo;
  fileInfoInitContents(&fileInfo);

  throws(tryReplCompile(stream, fileInfo, vm, &unit, error));
  printCodeUnit(&unit);

  throws(tryVMEval(vm, &unit, pool, &result, error));

  if (result.type == RT_RESULT) {
    throws(tryExprPrnStr(&result.result, outputText, error));
  }
  else {
    throws(tryExceptionPrintf(&result.exception, error));
  }

  ret = R_SUCCESS;
  goto done;

  failure:
  goto done;

  done:
    tryStreamFree(stream, error); // frees input stream also
    vmFreeContents(vm);
    codeUnitFreeContents(&unit);
    poolFree(pool);
    return ret;
}

RetVal tryReplEval(wchar_t *inputText, wchar_t **outputText, Error *error) {
  return tryReplEvalConf(inputText, outputText, true, error);
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

  InputStream_t source;
  TokenStream_t stream;
  FileInfo fileInfo;

  throws(tryFileInputStreamMakeFilename(filename, &source, error));
  throws(tryStreamMake(source, &stream, error));

  fileInfoInitContents(&fileInfo);
  char* baseFileName = basename(filename);
  fileInfo.hasFileName = true;
  throws(tryTextMakeFromChar(baseFileName, &fileInfo.fileName, error));

  CodeUnit unit;

  Pool_t pool = NULL;
  throws(tryPoolCreate(&pool, ONE_MB, error));

  VMEvalResult result;
  while (true) {

    codeUnitInitContents(&unit);

    ret = tryReplCompile(stream, fileInfo, vm, &unit, error);
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

    codeUnitFreeContents(&unit);
  }

  ret = R_SUCCESS;
  goto done;

  failure:
    goto done;

  done:
    codeUnitFreeContents(&unit);
    poolFree(pool);
    fileInfoFreeContents(&fileInfo);
    return ret;
}




































