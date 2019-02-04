#ifndef WARP_LANG_REPL_H
#define WARP_LANG_REPL_H

#include "compiler.h"

RetVal tryReplCompile(TokenStream_t stream, VM_t vm, CodeUnit *codeUnit, Error *error);

RetVal tryReplEval(wchar_t *inputText, wchar_t **outputText, Error *error);

RetVal tryLoad(VM_t vm, char *filename, Error *error);

#endif //WARP_LANG_REPL_H
