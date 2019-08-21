#ifndef WARP_LANG_AST_H
#define WARP_LANG_AST_H

#include <wchar.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include "source.h"
#include "utils.h"

struct Expr;

typedef struct ExprString {
  wchar_t *value;
  uint64_t length;
} ExprString;

typedef struct ExprNumber {
  uint64_t value;
} ExprNumber;

typedef struct ExprChar {
  wchar_t value;
} ExprChar;

typedef struct ExprSymbol {
  wchar_t *value;
  uint64_t length;
} ExprSymbol;

typedef struct ExprKeyword {
  wchar_t *value;
  uint64_t length;
} ExprKeyword;

typedef struct ExprBoolean {
  bool value;
} ExprBoolean;

typedef struct ListElement {
  struct Expr *expr;
  struct ListElement *next;
} ListElement;

typedef struct ExprList {
  uint64_t length;
  ListElement *head;
  ListElement *tail;
} ExprList;

typedef struct ExprVec {
  uint64_t length;
  ListElement *head;
  ListElement *tail;
} ExprVec;

typedef struct MapElement {
  struct Expr *key;
  struct Expr *value;
  struct MapElement *next;
} MapElement;

typedef struct ExprMap {
  uint64_t length;
  MapElement *head;
  MapElement *tail;
} ExprMap;

typedef enum ExprType {
  N_NONE,
  N_STRING,
  N_NUMBER,
  N_CHAR,
  N_SYMBOL,
  N_KEYWORD,
  N_BOOLEAN,
  N_NIL,
  N_LIST,
  N_VEC,
  N_MAP
} ExprType;

typedef struct Expr {
  ExprType type;
  union {
    ExprString string;
    ExprNumber number;
    ExprChar chr;
    ExprSymbol symbol;
    ExprKeyword keyword;
    ExprBoolean boolean;
    ExprList list;
    ExprVec vec;
    ExprMap map;
  };
  SourceLocation source;
} Expr;

Expr* exprMake(Pool_t pool);

Expr* stringMake(Pool_t pool, wchar_t *input, uint64_t length);
Expr* numberMake(Pool_t pool, uint64_t value);
Expr* charMake(Pool_t pool, wchar_t value);
Expr* symbolMake(Pool_t pool, wchar_t *name, uint64_t len);
Expr* keywordMake(Pool_t pool, wchar_t *name, uint64_t len);
Expr* booleanMake(Pool_t pool, bool value);
Expr* nilMake(Pool_t pool);
Expr* listMake(Pool_t pool);
Expr* vecMake(Pool_t pool);
Expr* mapMake(Pool_t pool);

void exprInitContents(Expr *expr);
void listInitContents(ExprList *list);
void listAppend(Pool_t pool, ExprList *list, Expr *expr);
void vecInitContents(ExprVec *list);
void vecAppend(Pool_t pool, ExprVec *list, Expr *expr);
void mapInitContents(ExprMap *map);
void mapPut(Pool_t pool, ExprMap *map, Expr *key, Expr *value);

void exprPrnBufConf(Expr *expr, StringBuffer_t b, bool readable);
void exprPrnBuf(Expr *expr, StringBuffer_t b);
wchar_t* exprPrnStr(Pool_t pool, Expr *expr);
void exprPrn(Pool_t pool, Expr* expr);


/*typedef struct Form Form;

//
// reader forms
//

typedef struct FormString {
  wchar_t *value;
  uint64_t length;
} FormString;

typedef struct FormNumber {
  uint64_t value;
} FormNumber;

typedef struct FormChar {
  wchar_t value;
} FormChar;

typedef struct FormSymbol {
  wchar_t *value;
  uint64_t length;
} FormSymbol;

typedef struct FormKeyword {
  wchar_t *value;
  uint64_t length;
} FormKeyword;

typedef struct FormBoolean {
  bool value;
} FormBoolean;

typedef struct ListElement {
  struct Form *expr;
  struct ListElement *next;
} ListElement;

typedef struct FormList {
  uint64_t length;
  ListElement *head;
  ListElement *tail;
} FormList;

typedef struct FormVec {
  uint64_t length;
  ListElement *head;
  ListElement *tail;
} FormVec;

typedef struct MapElement {
  struct Form *key;
  struct Form *value;
  struct MapElement *next;
} MapElement;

typedef struct FormMap {
  uint64_t length;
  MapElement *head;
  MapElement *tail;
} FormMap;

//
// analyzer forms
//

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

//
// `builtin` is a special form that allows code to invoke compile-target specific functionality.
// Analyzer does not interpret builtins, they are handled exclusively by the compiler/code emitter.
//
// (builtin :add 10 20)
//
typedef struct FormBuiltin {
  Text name;
  Forms args;
} FormBuiltin;

typedef struct FormHandler {
  Form *handler;
  Forms forms;
} FormHandler;

typedef enum FormType {
  F_NONE,
  F_STRING,
  F_NUMBER,
  F_CHAR,
  F_SYMBOL,
  F_KEYWORD,
  F_BOOLEAN,
  F_NIL,
  F_LIST,
  F_VEC,
  F_MAP,
  F_IF,
  F_LET,
  F_DEF,
  F_ENV_REF,
  F_VAR_REF,
  F_FN,
  F_BUILTIN,
  F_FN_CALL,
  F_HANDLER,
} FormType;

typedef struct Form {
  FormType type;
  union {
    FormString string;
    FormNumber number;
    FormChar chr;
    FormSymbol symbol;
    FormKeyword keyword;
    FormBoolean boolean;
    FormList list;
    FormVec vec;
    FormMap map;
    FormIf iff;
    FormLet let;
    FormDef def;
    FormEnvRef envRef;
    FormVarRef varRef;
    FormFn fn;
    FormFnCall fnCall;
    FormBuiltin builtin;
    FormHandler handler;
  };
  SourceLocation source;
} Form;

typedef struct FormRoot {
  BindingTable table;
  Form *form;
  Text fileName;
  bool hasFileName;
} FormRoot;
 */


#endif //WARP_LANG_AST_H
