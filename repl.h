#ifndef WARP_LANG_REPL_H
#define WARP_LANG_REPL_H

#include "compiler.h"

typedef struct {
  bool hasFileName;
  Text fileName;
} FileInfo;

void fileInfoInitContents(FileInfo *f);
void fileInfoFreeContents(FileInfo *f);

RetVal tryReplCompile(TokenStream_t stream, FileInfo fileInfo, VM_t vm, CodeUnit *codeUnit, Error *error);

RetVal tryReplEval(wchar_t *inputText, wchar_t **outputText, Error *error);

RetVal tryLoad(VM_t vm, char *filename, Error *error);

#endif //WARP_LANG_REPL_H
