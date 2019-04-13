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

RetVal tryReplCompile(TokenStream_t stream, FileInfo fileInfo, VM_t vm, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  Expr *expr = NULL;
  FormRoot *form = NULL;
  Expander_t expander = NULL;

  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);

  throws(tryMakeExpander(&expander, vm, error));
  options.expander = expander;

  if (fileInfo.hasFileName) {
    options.hasFileName = true;
    options.fileName = fileInfo.fileName;
  }

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

RetVal tryReplEvalConf(wchar_t *inputText, wchar_t **outputText, bool useStdLib, Error *error) {

  RetVal ret;

  CodeUnit unit;
  InputStream_t source = NULL;
  TokenStream_t stream = NULL;
  VM_t vm = NULL;
  VMEvalResult result;

  printf("%ls\n", inputText);

  codeUnitInitContents(&unit);

  throws(tryStringInputStreamMake(inputText, wcslen(inputText), &source, error));
  throws(tryStreamMake(source, &stream, error));
  throws(tryVMMake(&vm, error));

  if (useStdLib) {
//    char *stdLib = "/Users/ddcmhenry/dev/funtastic/branches/warp-lang/core.lsp";
//    char *stdLib = "/warp/core.lsp";
    throws(tryLoad(vm, STD_LIB, error));
  }

  FileInfo fileInfo;
  fileInfoInitContents(&fileInfo);

  throws(tryReplCompile(stream, fileInfo, vm, &unit, error));
  printCodeUnit(&unit);

  throws(tryVMEval(vm, &unit, &result, error));

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
    evalResultFreeContents(&result);
    tryStreamFree(stream, error); // frees input stream also
    vmFreeContents(vm);
    codeUnitFreeContents(&unit);
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

    throws(tryVMEval(vm, &unit, &result, error));

    codeUnitFreeContents(&unit);
    evalResultFreeContents(&result);
  }

  ret = R_SUCCESS;
  goto done;

  failure:
    goto done;

  done:
    codeUnitFreeContents(&unit);
    evalResultFreeContents(&result);
    fileInfoFreeContents(&fileInfo);
    return ret;
}




































