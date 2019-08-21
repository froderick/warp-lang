#ifndef WARP_LANG_ANALYZER_H
#define WARP_LANG_ANALYZER_H

#include <stdint.h>
#include <stdlib.h>

#include "../errors.h"
#include "ast.h"
#include "expander.h"
#include "pool.h"

// TODO: need to support 'ns' special form



RetVal tryFormAnalyze(Form* expr, Pool_t pool, FormRoot **form, Error *error);

typedef struct AnalyzeOptions {
  Expander_t expander;
  bool hasFileName;
  Text fileName;
} AnalyzeOptions;

void analyzeOptionsInitContents(AnalyzeOptions *options);
RetVal tryFormAnalyzeOptions(AnalyzeOptions options, Form* expr, Pool_t pool, FormRoot **ptr, Error *error);

void rootInitContents(FormRoot *root);
void fnCallInitContents(FormFnCall *fnCall);

#endif //WARP_LANG_ANALYZER_H

