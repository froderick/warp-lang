#ifndef WARP_LANG_REPL_H
#define WARP_LANG_REPL_H

#include "lexer.h"
#include "compiler.h"
#include "../vm/vm.h"

typedef struct {
  bool hasFileName;
  Text fileName;
} FileInfo;

void fileInfoInitContents(FileInfo *f);

RetVal tryReplCompile(Pool_t outputPool, TokenStream_t stream, FileInfo fileInfo, VM_t vm, CodeUnit *codeUnit, Error *error);
RetVal tryReplEvalConf(Pool_t outputPool, wchar_t *inputText, wchar_t **outputText, bool useStdLib, VMConfig config, Error *error);
RetVal tryLoad(VM_t vm, char *filename, Error *error);

#endif //WARP_LANG_REPL_H
