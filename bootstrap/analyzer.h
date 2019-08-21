#ifndef WARP_LANG_ANALYZER_H
#define WARP_LANG_ANALYZER_H

#include <stdint.h>
#include <stdlib.h>

#include "../errors.h"
#include "ast.h"
#include "expander.h"
#include "pool.h"

// TODO: need to support 'ns' special form

typedef enum BindingSource {
  BS_NONE,
  BS_CAPTURED,
  BS_LOCAL,
} BindingSource;

typedef enum LocalBindingType {
  BT_NONE,
  BT_LET,
  BT_FN_REF,
  BT_FN_ARG,
} LocalBindingType;

typedef struct LocalBindingInfo {
  LocalBindingType type;
  uint16_t typeIndex; // each type of binding is numbered so it can be referenced
} LocalBindingInfo;

typedef struct CapturedBindingInfo {
  uint16_t bindingIndex;
} CapturedBindingInfo;

typedef struct Binding {
  Text name;
  BindingSource source;
  union {
    LocalBindingInfo local;
    CapturedBindingInfo captured;
  };
} Binding;

typedef struct BindingTable {
  uint16_t allocatedSpace;
  uint16_t usedSpace;
  Binding *bindings;
} BindingTable;

typedef struct Form Form;

typedef struct Forms {
  uint16_t numForms;
  Form *forms;
} Forms;

typedef struct FormIf {
  Form *test;
  Form *ifBranch;
  Form *elseBranch;
} FormIf;

typedef struct LetBinding {
  Text name;
  SourceLocation source;
  Form *value;
  uint16_t bindingIndex;
} LetBinding;

typedef struct FormLet {
  Text *name;
  LetBinding *bindings;
  uint16_t numBindings;
  Forms forms;
} FormLet;

typedef struct FormDef {
  Text name;
  Form *value;
} FormDef;

typedef struct FormEnvRef {
  uint64_t bindingIndex; // this is an typeIndex into the binding table for this reference's lexical scope
} FormEnvRef;

typedef struct FormVarRef {
  Text name;
} FormVarRef;

typedef struct FormFnArg {
  Text name;
  SourceLocation source;
  uint16_t bindingIndex;
} FormFnArg;

typedef struct FormFn {

  BindingTable table;

  bool isClosure;
  uint16_t numCaptures;

  // this name is only used within the function to refer to itself, for things like recursion
  bool hasName;
  Text name;
  uint16_t bindingIndex;

  FormFnArg *args;
  uint16_t numArgs;
  Forms forms;

  bool usesVarArgs;

} FormFn;

typedef struct FormFnCall {
  Form *fnCallable;
  Forms args;
  bool tailPosition;
  bool recurses;
} FormFnCall;

/*
 * `builtin` is a special form that allows code to invoke compile-target specific functionality.
 * Analyzer does not interpret builtins, they are handled exclusively by the compiler/code emitter.
 *
 * (builtin :add 10 20)
 */
typedef struct FormBuiltin {
  Text name;
  Forms args;
} FormBuiltin;

typedef struct FormList {
  Forms forms;
} FormList;

typedef struct FormVec {
  Forms forms;
} FormVec;

typedef struct FormHandler {
  Form *handler;
  Forms forms;
} FormHandler;

typedef enum FormType {
  F_NONE,
  F_CONST,
  F_IF,
  F_LET,
  F_DEF,
  F_ENV_REF,
  F_VAR_REF,
  F_FN,
  F_BUILTIN,
  F_FN_CALL,
  F_LIST,
  F_VEC,
  F_HANDLER,
} FormType;

typedef struct Form {
  FormType type;
  union {
    Expr *constant;
    FormIf iff;
    FormLet let;
    FormDef def;
    FormEnvRef envRef;
    FormVarRef varRef;
    FormFn fn;
    FormFnCall fnCall;
    FormBuiltin builtin;
    FormList list;
    FormList vec;
    FormHandler handler;
  };
  SourceLocation source;
} Form;

void formInitContents(Form *form);
void formsInitContents(Forms *forms);

typedef struct FormRoot {
  BindingTable table;
  Form *form;
  Text fileName;
  bool hasFileName;
} FormRoot;

RetVal tryFormAnalyze(Expr* expr, Pool_t pool, FormRoot **form, Error *error);

typedef struct AnalyzeOptions {
  Expander_t expander;
  bool hasFileName;
  Text fileName;
} AnalyzeOptions;

void analyzeOptionsInitContents(AnalyzeOptions *options);
RetVal tryFormAnalyzeOptions(AnalyzeOptions options, Expr* expr, Pool_t pool, FormRoot **ptr, Error *error);

void rootInitContents(FormRoot *root);
void fnCallInitContents(FormFnCall *fnCall);

#endif //WARP_LANG_ANALYZER_H

