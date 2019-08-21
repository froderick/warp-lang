#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>

#include "analyzer.h"
#include "utils.h"
#include "expander.h"
#include "pool.h"
#include "print.h"

/*
 * Core analyzer behavior.
 */

typedef struct BindingTables {
  uint64_t allocatedSpace;
  uint64_t usedSpace;
  BindingTable **tables;
} BindingTables;

typedef struct ResolverBinding {
  uint16_t tableIndex;
  uint16_t bindingIndex;
  Binding *binding;
} ResolverBinding;

typedef struct ResolverStack {
  uint64_t allocatedSpace;
  uint64_t usedSpace;
  ResolverBinding *bindings;
} ResolverStack;

typedef struct AnalyzerContext {
  Pool_t pool;
  AnalyzeOptions options;
  BindingTables bindingTables;
  ResolverStack resolverStack;
  bool inQuote;
  bool inSyntaxQuote;
} AnalyzerContext;

void formInitContents(Form *form) {
  form->type = F_NONE;
  sourceLocationInitContents(&form->source);
}

void bindingInitContents(Binding *binding) {
  textInitContents(&binding->name);
  binding->source = BS_NONE;
}

void bindingTableInitContents(BindingTable *table) {
  table->bindings = NULL;
  table->usedSpace = 0;
  table->allocatedSpace = 0;
}

void bindingTablesInitContents(BindingTables *tables) {
  tables->usedSpace = 0;
  tables->allocatedSpace = 0;
  tables->tables = NULL;
}

void addBinding(Pool_t pool, BindingTable *table, Binding binding) {
  if (table->bindings == NULL) {
    uint16_t len = 16;
    palloc(pool, table->bindings, len * sizeof(Binding), "Binding array");
    table->allocatedSpace = len;
  }
  else if (table->usedSpace == table->allocatedSpace) {
    uint64_t newAllocatedLength = table->allocatedSpace * 2;

    Binding *resized = NULL;
    palloc(pool, resized, newAllocatedLength * sizeof(Binding), "Binding array");
    memcpy(resized, table->bindings, table->usedSpace * sizeof(Binding));

    table->allocatedSpace = newAllocatedLength;
    table->bindings = resized;
  }

  uint64_t index = table->usedSpace;
  table->bindings[index] = binding;
  table->usedSpace = index + 1;
}

void pushBindingTable(Pool_t pool, BindingTables *tables, BindingTable *table) {
  if (tables->tables == NULL) {
    uint16_t len = 16;
    palloc(pool, tables->tables, len * sizeof(BindingTable*), "BindingTable pointer array");
    tables->allocatedSpace = len;
  }
  else if (tables->usedSpace == tables->allocatedSpace) {
    uint64_t newAllocatedLength = tables->allocatedSpace * 2;

    BindingTable **resized = NULL;
    palloc(pool, resized, newAllocatedLength * sizeof(BindingTable*), "BindingTable pointer array");
    memcpy(resized, tables->tables, tables->usedSpace * sizeof(BindingTable*));

    tables->allocatedSpace = newAllocatedLength;
    tables->tables = resized;
  }

  uint64_t index = tables->usedSpace;
  tables->tables[index] = table;
  tables->usedSpace = index + 1;
}

void popBindingTable(BindingTables *tables) {
  if (tables->usedSpace == 0) {
    explode("cannot pop binding table from empty stack");
  }
  tables->tables[tables->usedSpace - 1] = NULL;
  tables->usedSpace = tables->usedSpace - 1;
}

Binding* findBinding(BindingTables *tables, wchar_t *bindingName) {

  for (uint16_t i=0; i<tables->usedSpace; i++) {
    BindingTable *table = tables->tables[tables->usedSpace - (i + 1)];

    for (uint16_t j=0; j<table->usedSpace; j++) {
      Binding *b = &table->bindings[table->usedSpace - (j + 1)];

      if (wcscmp(bindingName, b->name.value) == 0) {
        return b;
      }
    }
  }

  return NULL;
}

void resolverBindingInitContents(ResolverBinding *b) {
  b->tableIndex = 0;
  b->binding = NULL;
}

void resolverStackInitContents(ResolverStack *stack) {
  stack->usedSpace = 0;
  stack->allocatedSpace = 0;
  stack->bindings = NULL;
}

void pushResolverBinding(Pool_t pool, ResolverStack *stack, ResolverBinding binding) {

  if (stack->bindings == NULL) {
    uint16_t len = 16;
    palloc(pool, stack->bindings, len * sizeof(ResolverBinding), "ResolverBinding array");
    stack->allocatedSpace = len;
  }
  else if (stack->usedSpace == stack->allocatedSpace) {
    uint64_t newAllocatedLength = stack->allocatedSpace * 2;

    ResolverBinding *resized = NULL;
    palloc(pool, resized, newAllocatedLength * sizeof(ResolverBinding), "BindingTable pointer array");
    memcpy(resized, stack->bindings, stack->usedSpace * sizeof(ResolverBinding));

    stack->allocatedSpace = newAllocatedLength;
    stack->bindings = resized;
  }

  uint64_t index = stack->usedSpace;
  stack->bindings[index] = binding;
  stack->usedSpace = index + 1;
}

void popResolverBindings(ResolverStack *stack, uint16_t numBindings) {
  if (stack->usedSpace < numBindings) {
    explode("cannot pop more bindings than exist");
  }
  stack->usedSpace = stack->usedSpace - numBindings;
}

ResolverBinding* findResolverBinding(ResolverStack *stack, wchar_t *bindingName) {
  for (uint16_t i=0; i<stack->usedSpace; i++) {
    ResolverBinding *b = &stack->bindings[stack->usedSpace - (i + 1)];
    if (wcscmp(bindingName, b->binding->name.value) == 0) {
      return b;
    }
  }
  return NULL;
}

uint16_t getCurrentBindingTableIndex(AnalyzerContext *ctx) {
  if (ctx->bindingTables.usedSpace == 0) {
    explode("no current binding table found");
  }
  return ctx->bindingTables.usedSpace - 1;
}

void createBinding(AnalyzerContext *ctx, Binding binding, uint16_t *bindingIndexPtr) {
  if (ctx->bindingTables.usedSpace == 0) {
    explode("no current binding table found");
  }

  uint16_t tableIndex = ctx->bindingTables.usedSpace - 1;
  BindingTable *table = ctx->bindingTables.tables[tableIndex];
  addBinding(ctx->pool, table, binding);
  uint16_t bindingIndex = table->usedSpace - 1;
  Binding *b = &table->bindings[bindingIndex];

  ResolverBinding resolver;
  resolverBindingInitContents(&resolver);
  resolver.tableIndex = tableIndex;
  resolver.bindingIndex = bindingIndex;
  resolver.binding = b;

  pushResolverBinding(ctx->pool, &ctx->resolverStack, resolver);

  *bindingIndexPtr = bindingIndex;
}

void popBindings(AnalyzerContext *ctx, uint16_t numBindings) {
  popResolverBindings(&ctx->resolverStack, numBindings);
}

/*
 * Analyzers for the different types of forms.
 */

RetVal _tryFormAnalyze(AnalyzerContext *ctx, Expr* expr, Form **ptr, Error *error);
RetVal tryFormAnalyzeContents(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error);

uint64_t getExprPosition(Expr *expr) {
  return expr->source.position;
}

uint64_t getFormPosition(Form *form) {
  return form->source.position;
}

RetVal tryIfAnalyze(AnalyzerContext *ctx, Expr* ifExpr, FormIf *iff, Error *error) {

  iff->test = NULL;
  iff->ifBranch = NULL;
  iff->elseBranch = NULL;

  RetVal ret;

  uint64_t pos = getExprPosition(ifExpr);
  if (ifExpr->list.length < 2) {
    throwSyntaxError(error, pos, "the 'if' special form requires a test");
  }
  if (ifExpr->list.length < 3) {
    throwSyntaxError(error, pos, "the 'if' special form requires at least one return value");
  }
  if (ifExpr->list.length > 4) {
    throwSyntaxError(error, pos, "the 'if' special form only supports an 'if' and 'else' branch");
  }

  Expr *testExpr = ifExpr->list.head->next->expr;
  Expr *ifBranchExpr = ifExpr->list.head->next->next->expr;

  throws(_tryFormAnalyze(ctx, testExpr, &iff->test, error));
  throws(_tryFormAnalyze(ctx, ifBranchExpr, &iff->ifBranch, error));

  bool hasElse = ifExpr->list.length == 4;
  if (hasElse) {
    Expr *elseBranchExpr = ifExpr->list.head->next->next->next->expr;
    throws(_tryFormAnalyze(ctx, elseBranchExpr, &iff->elseBranch, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

/*
 * purposes of the environment stack tracking
 * - determine the value of a binding reference, by matching its name to the nearest binding by that name in the stack
 * - the value includes the type of the reference (arg or local) as well as the typeIndex by which it can be identified
 */

void formsInitContents(Forms *forms) {
  forms->numForms = 0;
  forms->forms = NULL;
}

void formsAllocate(Pool_t pool, Forms *forms, uint16_t length) {
  forms->numForms = length;
  palloc(pool, forms->forms, sizeof(Form) * forms->numForms, "Forms array");
  for (uint16_t i=0; i<forms->numForms; i++) {
      formInitContents(&forms->forms[i]);
  }
}

void letInitContents(FormLet *let) {
  let->name = NULL;
  let->numBindings = 0;
  let->bindings = NULL;
  formsInitContents(&let->forms);
}

void letBindingInitContents(LetBinding *b) {
  textInitContents(&b->name);
  sourceLocationInitContents(&b->source);
  b->value = NULL;
}

RetVal tryLetAnalyze(AnalyzerContext *ctx, Expr* letExpr, FormLet *let, Error *error) {
  RetVal ret;

  letInitContents(let);
  uint64_t pos = getExprPosition(letExpr);
  ListElement *next = letExpr->list.head->next;

  if (next == NULL) {
    throwSyntaxError(error, pos, "the 'let' special form requires at least one parameter");
  }

  // sanity checking
  Expr *bindingsExpr = next->expr;
  if (bindingsExpr->type != N_NIL) {
    if (bindingsExpr->type != N_LIST) {
      throwSyntaxError(error, pos, "the 'let' special form requires the first parameter to be a list");
    }
    if (bindingsExpr->list.length % 2 != 0) {
      throwSyntaxError(error, pos,
                       "the 'let' special form requires the first parameter to be a list with an even number of arguments");
    }

    // create the bindings
    let->numBindings = bindingsExpr->list.length / 2;
    tryPalloc(ctx->pool, let->bindings, sizeof(LetBinding) * let->numBindings, "LetBinding array");

    // add the bindings to the current binding table
    // push the bindings on the current binding stack

    // initialize the bindings
    ListElement *bindingElem = bindingsExpr->list.head;
    for (int i = 0; bindingElem != NULL; i++) {

      if (bindingElem->expr->type != N_SYMBOL) {
        throwSyntaxError(error, pos, "only symbols can be bound as names");
      }

      LetBinding *b = let->bindings + i;
      letBindingInitContents(b);
      throws(
          tryTextMake(ctx->pool, bindingElem->expr->symbol.value, &b->name, bindingElem->expr->symbol.length, error));
      b->source = bindingElem->expr->source;
      throws(_tryFormAnalyze(ctx, bindingElem->next->expr, &b->value, error));

      Binding binding;
      bindingInitContents(&binding);
      throws(tryTextCopy(ctx->pool, &b->name, &binding.name, error));
      binding.source = BS_LOCAL;
      binding.local.type = BT_LET;
      binding.local.typeIndex = i;

      createBinding(ctx, binding, &b->bindingIndex);

      bindingElem = bindingElem->next->next;
    }
    next = next->next;
  }

  // create the forms within this lexical scope
  uint64_t numForms = 0;
  for (ListElement *e = next; e != NULL; e = e->next) {
    numForms++;
  }

  formsAllocate(ctx->pool, &let->forms, numForms);

  ListElement *exprElem = next;
  for (int i=0; i<let->forms.numForms; i++) {
    Form *thisForm = let->forms.forms + i;
    throws(tryFormAnalyzeContents(ctx, exprElem->expr, thisForm, error));
    exprElem = exprElem->next;
  }

  // discard the registered bindings from the environment stack
  popBindings(ctx, let->numBindings);

  return R_SUCCESS;
  failure:
    return ret;
}

void defInitContents(FormDef *def) {
  textInitContents(&def->name);
  def->value = NULL;
}

RetVal tryDefAnalyze(AnalyzerContext *ctx, Expr* defExpr, FormDef *def, Error *error) {
  RetVal ret;

  defInitContents(def);

  // sanity checking
  uint64_t pos = getExprPosition(defExpr);
  if (defExpr->list.length < 2) {
    throwSyntaxError(error, pos, "the 'def' special form requires at least one parameter");
  }
  if (defExpr->list.length > 3) {
    throwSyntaxError(error, pos, "the 'def' special form takes at most two parameters");
  }
  Expr *symbol = defExpr->list.head->next->expr;
  if (symbol->type != N_SYMBOL) {
    throwSyntaxError(error, pos, "the 'let' special form requires the first parameter to be a symbol");
  }

  throws(tryTextMake(ctx->pool, symbol->symbol.value, &def->name, symbol->symbol.length, error));

  if (defExpr->list.length == 3) {
    throws(_tryFormAnalyze(ctx, defExpr->list.head->next->next->expr, &def->value, error));
  }

  // update the analyzer state so it knows about this def
  //  throws(tryDefVar(analyzer, def->name, def->nameLength, def->value, error));

  return R_SUCCESS;

  failure:
    return ret;
}

void formFnInitContents(FormFn *fn) {
  bindingTableInitContents(&fn->table);
  fn->hasName = false;
  textInitContents(&fn->name);
  fn->args = NULL;
  fn->numArgs = 0;
  formsInitContents(&fn->forms);
  fn->usesVarArgs = false;
}

void fnArgInitContents(FormFnArg *arg) {
  textInitContents(&arg->name);
  sourceLocationInitContents(&arg->source);
}

RetVal tryFnValidateArgs(Expr *argsExpr, uint16_t *numArgsPtr, bool *varArgsPtr, Error *error) {
  RetVal ret;

  uint16_t numArgs = 0;
  bool varArgs = false;

  uint64_t pos = getExprPosition(argsExpr);

  ListElement *argElem = argsExpr->list.head;
  bool seenLast = false;
  for (uint16_t i=0; i<argsExpr->list.length; i++) {

    if (seenLast) {
      throwSyntaxError(error, argsExpr->source.position, "unexpected argument: %ls", argElem->expr->symbol.value);
    }

    if (wcscmp(argElem->expr->symbol.value, L"&") == 0) { // var-args &
      if (varArgs) {
        throwSyntaxError(error, argsExpr->source.position, "unexpected argument: &");
      }
      varArgs = true;
    }
    else { // acutal argument
      if (argElem->expr->type != N_SYMBOL) {
        throwSyntaxError(error, pos, "only symbols can be used as function arguments");
      }
      if (varArgs) {
        seenLast = true;
      }
      numArgs++;
    }

    argElem = argElem->next;
  }

  *numArgsPtr = numArgs;
  *varArgsPtr = varArgs;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryFnParseArgs(Pool_t pool, Expr *argsExpr, FormFn *fn, Error *error) {
  RetVal ret;

  uint64_t pos = getExprPosition(argsExpr);

  if (argsExpr->type == N_LIST) {

    throws(tryFnValidateArgs(argsExpr, &fn->numArgs, &fn->usesVarArgs, error));
    tryPalloc(pool, fn->args, sizeof(FormFnArg) * fn->numArgs, "FormFnArg array");

    ListElement *argElem = argsExpr->list.head;
    for (int i=0; i<fn->numArgs; i++) {

      if (i + 1 == fn->numArgs && fn->usesVarArgs) {
        argElem = argElem->next; // skip &
      }

      FormFnArg *arg = fn->args + i;
      fnArgInitContents(arg);
      throws(tryTextMake(pool, argElem->expr->symbol.value, &arg->name, argElem->expr->symbol.length, error));
      arg->source = argElem->expr->source;

      argElem = argElem->next;
    }
  }
  else if (argsExpr->type == N_NIL) {
    // leave args empty
  }
  else {
    throwSyntaxError(error, pos, "the 'fn' requires an arg list of the type N_LIST or N_NIL: %u",
                     argsExpr->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryFnParse(Pool_t pool, Expr* fnExpr, FormFn *fn, Expr **formElements, Error *error) {
  RetVal ret;

  uint64_t pos = getExprPosition(fnExpr);
  ListElement *itr = fnExpr->list.head->next;

  if (itr == NULL) {
    throwSyntaxError(error, pos, "the 'fn' special form requires at least one parameter");
  }

  // the optional function name
  if (itr->expr->type == N_SYMBOL) {
    throws(tryTextMake(pool, itr->expr->symbol.value, &fn->name, itr->expr->symbol.length, error));
    fn->hasName = true;
    itr = itr->next;
  }

  // create the arguments
  if (itr == NULL) {
    throwSyntaxError(error, pos, "the 'fn' special form requires an argument list");
  }

  Expr *argsExpr = itr->expr;
  throws(tryFnParseArgs(pool, argsExpr, fn, error));
  itr = itr->next;

  uint16_t nonFormElems = 2; // 'fn' and 'args list'
  if (fn->hasName) {
    nonFormElems = nonFormElems + 1; // optional function name
  }

  fn->forms.numForms = fnExpr->list.length - nonFormElems;
  tryPalloc(pool, *formElements, sizeof(Expr) * fn->forms.numForms, "Expr array");

  for (int i=0; i<fn->forms.numForms; i++) {
    *formElements[i] = *itr->expr;
    itr = itr->next;
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void _markTailCalls(Form *last) {
  switch (last->type) {
    case F_IF:
      _markTailCalls(last->iff.ifBranch);
      if (last->iff.elseBranch != NULL) {
        _markTailCalls(last->iff.elseBranch);
      }
      break;
    case F_LET: {
      if (last->let.forms.numForms > 0) {
        Form *letLast = &last->let.forms.forms[last->let.forms.numForms - 1];
        _markTailCalls(letLast);
      }
      break;
    }
    case F_FN_CALL:
      last->fnCall.tailPosition = true;
      break;
    default:
      break;
  }
}

void markTailCalls(FormFn *fn) {
  if (fn->forms.numForms > 0) {
    Form *last = &fn->forms.forms[fn->forms.numForms- 1];
    _markTailCalls(last);
  }
}

uint16_t bindingTableCaptures(BindingTable *table) {
  uint16_t numCaptures = 0;
  for (uint16_t i=0; i<table->usedSpace; i++) {
    Binding *b = &table->bindings[i];
    if (b->source == BS_CAPTURED) {
      numCaptures++;
    }
  }
  return numCaptures;
}

RetVal tryFnAnalyze(AnalyzerContext *ctx, Expr* fnExpr, FormFn *fn, Error *error) {
  RetVal ret;

  // things that get cleaned up always
  Expr *formElements = NULL;

  formFnInitContents(fn);
  throws(tryFnParse(ctx->pool, fnExpr, fn, &formElements, error));

  // create new binding stack, initialized with the fn args as the first bindings

  pushBindingTable(ctx->pool, &ctx->bindingTables, &fn->table);
  uint16_t numBindingsPushed = 0;

  if (fn->hasName) {

    Binding binding;
    bindingInitContents(&binding);
    throws(tryTextCopy(ctx->pool, &fn->name, &binding.name, error));
    binding.source = BS_LOCAL;
    binding.local.type = BT_FN_REF;
    binding.local.typeIndex = 0;

    createBinding(ctx, binding, &fn->bindingIndex);
    numBindingsPushed = numBindingsPushed + 1;
  }

  for (uint64_t i=0; i<fn->numArgs; i++) {

    Binding binding;
    bindingInitContents(&binding);
    throws(tryTextCopy(ctx->pool, &fn->args[i].name, &binding.name, error));
    binding.source = BS_LOCAL;
    binding.local.type = BT_FN_ARG;
    binding.local.typeIndex = i;

    createBinding(ctx, binding, &fn->args[i].bindingIndex);
    numBindingsPushed = numBindingsPushed + 1;
  }

  // create the forms within this fn lexical scope
  formsAllocate(ctx->pool, &fn->forms, fn->forms.numForms);
  for (int i=0; i<fn->forms.numForms; i++) {
    Expr expr = formElements[i];
    Form *thisForm = fn->forms.forms + i;
    throws(tryFormAnalyzeContents(ctx, &expr, thisForm, error));
  }

  markTailCalls(fn);

  fn->numCaptures = bindingTableCaptures(&fn->table);
  fn->isClosure = fn->numCaptures > 0;

  popBindings(ctx, numBindingsPushed);
  popBindingTable(&ctx->bindingTables);

  return R_SUCCESS;
  failure:
    return ret;
}

void builtinInitContents(FormBuiltin *builtin) {
  textInitContents(&builtin->name);
  formsInitContents(&builtin->args);
}

RetVal tryBuiltinAnalyze(AnalyzerContext *ctx, Expr *expr, FormBuiltin *builtin, Error *error) {

  RetVal ret;

  builtinInitContents(builtin);

  Expr *name = expr->list.head->next->expr;
  if (name->type != N_KEYWORD) {
    throwSyntaxError(error, name->source.position, "the 'builtin' special form requires the first parameter to be a keyword");
  }

  throws(tryTextMake(ctx->pool, name->keyword.value, &builtin->name, name->keyword.length, error));

  formsAllocate(ctx->pool, &builtin->args, expr->list.length - 2);

  ListElement *argExpr = expr->list.head->next->next;
  for (int i=0; i<builtin->args.numForms; i++) {
    Form *arg = builtin->args.forms + i;
    throws(tryFormAnalyzeContents(ctx, argExpr->expr, arg, error));
    argExpr = argExpr->next;
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void _listInitContents(FormList *list) {
  formsInitContents(&list->forms);
}

RetVal tryEnvRefAnalyze(AnalyzerContext *ctx, Expr *expr, uint16_t bindingIndex, FormEnvRef *envRef, Error *error) {
  envRef->bindingIndex = bindingIndex;
  return R_SUCCESS;
}

void varRefInitContents(FormVarRef *varRef) {
  textInitContents(&varRef->name);
}

RetVal tryVarRefAnalyze(AnalyzerContext *ctx, Expr *expr, FormVarRef *varRef, Error *error) {
  RetVal ret;

  varRefInitContents(varRef);

  if (expr->type != N_SYMBOL) {
    throwSyntaxError(error, getExprPosition(expr), "var refs must be symbols: '%i'", expr->type);
  }

  throws(tryTextMake(ctx->pool, expr->symbol.value, &varRef->name, expr->symbol.length, error));

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal assertFnCallable(Form *form, Error *error) {

  RetVal ret;

  switch (form->type) {

    case F_IF:
    case F_LET:
    case F_FN:
    case F_BUILTIN:
    case F_FN_CALL:
    case F_ENV_REF:
      break;

    case F_VAR_REF:
//      if (form->varRef.var->value->type != F_FN) {
//        throwSyntaxError(error, getFormPosition(form),
//                         "var's value does not contain a function: '%ls'", form->varRef.var->name);
//      }
      break;

    case F_CONST:
      if (form->constant->type != N_KEYWORD) {
        throwSyntaxError(error, getFormPosition(form),
                         "first list element cannot be called as a function: '%i'", form->constant->type);
      }
      break;

    default:
      throwInternalError(error, "unhandled case %i", form->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void fnCallInitContents(FormFnCall *fnCall) {
  fnCall->fnCallable = NULL;
  formsInitContents(&fnCall->args);
  fnCall->tailPosition = false;
  fnCall->recurses = false;
}

RetVal tryFnCallAnalyze(AnalyzerContext *ctx, Expr *expr, FormFnCall *fnCall, Error *error) {

  RetVal ret;

  fnCallInitContents(fnCall);

  throws(_tryFormAnalyze(ctx, expr->list.head->expr, &fnCall->fnCallable, error));
  throws(assertFnCallable(fnCall->fnCallable, error));
  formsAllocate(ctx->pool, &fnCall->args, expr->list.length - 1);

  ListElement *argExpr = expr->list.head->next;
  for (int i=0; i<fnCall->args.numForms; i++) {
    Form *arg = fnCall->args.forms + i;
    throws(tryFormAnalyzeContents(ctx, argExpr->expr, arg, error));
    argExpr = argExpr->next;
  }

  /*
   * if we are inside a function, and if the fnCallable is an env-var that points to
   * this function refeference, this is recursion
   */

  if (fnCall->fnCallable->type == F_ENV_REF) {

    FormEnvRef *envRef = &fnCall->fnCallable->envRef;

    uint16_t currentTableIndex = getCurrentBindingTableIndex(ctx);
    BindingTable *current = ctx->bindingTables.tables[currentTableIndex];
    Binding *binding = &current->bindings[envRef->bindingIndex];

    if (binding->source == BS_LOCAL) {
      if (binding->local.type == BT_FN_REF) {
        fnCall->recurses = true;
      }
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryQuoteAnalyze(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error) {

  RetVal ret;

  if (expr->list.length != 2) {
    throwSyntaxError(error, expr->source.position, "wrong number of args passed to quote (%" PRIu64 " instead of 1)",
                     expr->list.length - 1);
  }

  bool lastInQuote = ctx->inQuote;
  ctx->inQuote = true;
  throws(tryFormAnalyzeContents(ctx, expr->list.head->next->expr, form, error));
  ctx->inQuote = lastInQuote;

  return R_SUCCESS;
  failure:
    return ret;
}

bool isSplicingUnquote(Expr *elem) {
  return
      elem->type == N_LIST
      && elem->list.length > 0
      && elem->list.head->expr->type == N_SYMBOL
      && wcscmp(elem->list.head->expr->symbol.value, L"splicing-unquote") == 0;
}

uint16_t numSyntaxQuotedListArgs(Expr *quoted) {

  uint16_t numArgs = 0;
  uint16_t listElementsSeen = 0;

  ListElement *argExpr = quoted->list.head;
  for (int i = 0; i < quoted->list.length ; i++) {
    Expr *elem = argExpr->expr;

    if (isSplicingUnquote(elem)) {
      if (listElementsSeen > 0) {
        numArgs++;
        listElementsSeen = 0;
      }
      numArgs++;
    }
    else {
      listElementsSeen++;
    }

    argExpr = argExpr->next;
  }

  if (listElementsSeen > 0) {
    numArgs++;
  }

  return numArgs;
}

RetVal trySyntaxQuoteListAnalyze(AnalyzerContext *ctx, Expr *quoted, Form *form, Error *error) {
  RetVal ret;

  uint16_t numArgs = numSyntaxQuotedListArgs(quoted);

  { // init fn call to concat
    FormVarRef ref;
    varRefInitContents(&ref);
    wchar_t *concat = L"concat";
    throws(tryTextMake(ctx->pool, concat, &ref.name, wcslen(concat), error));

    Form *fnCallable;
    tryPalloc(ctx->pool, fnCallable, sizeof(Form), "Form");
    formInitContents(fnCallable);
    fnCallable->type = F_VAR_REF;
    fnCallable->varRef = ref;

    formInitContents(form);
    form->type = F_FN_CALL;
    fnCallInitContents(&form->fnCall);
    form->fnCall.fnCallable = fnCallable;
    formsAllocate(ctx->pool, &form->fnCall.args, numArgs);
  }

  uint16_t nextArg = 0;
  ListElement *elem= quoted->list.head;
  while (elem != NULL) {

    Expr *elemExpr = elem->expr;
    Form *args = form->fnCall.args.forms;

    bool argInitialized = args[nextArg].type != F_NONE;

    if (isSplicingUnquote(elemExpr)) {
      if (argInitialized) {
        nextArg++;
      }
      bool last = ctx->inSyntaxQuote;
      ctx->inSyntaxQuote = false;
      throws(tryFormAnalyzeContents(ctx, elemExpr->list.head->next->expr, &args[nextArg], error));
      ctx->inSyntaxQuote = last;
      nextArg++;
    } else {

      Form *listContainer = &args[nextArg];

      if (listContainer->type == F_NONE) {
        listContainer->type = F_LIST;
        _listInitContents(&listContainer->list);
        formsAllocate(ctx->pool, &listContainer->list.forms, quoted->list.length); // allocate max it could ever be
        listContainer->list.forms.numForms = 0;                                    // pretend it is empty
      }

      if (args[nextArg].type != F_LIST) {
        throwSyntaxError(error, quoted->source.position, "this argument should have been a list");
      }

      uint16_t index = listContainer->list.forms.numForms;
      throws(tryFormAnalyzeContents(ctx, elemExpr, &listContainer->list.forms.forms[index], error));
      listContainer->list.forms.numForms++;

    }

    elem = elem->next;
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal trySyntaxQuoteAnalyze(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error) {
  RetVal ret;

  if (expr->list.length != 2) {
    throwSyntaxError(error, expr->source.position, "wrong number of args passed to syntax-quote (%" PRIu64
                     " instead of 1)", expr->list.length - 1);
  }

  bool lastValue = ctx->inSyntaxQuote;
  ctx->inSyntaxQuote = true;

  Expr *quoted = expr->list.head->next->expr;
  throws(tryFormAnalyzeContents(ctx, quoted, form, error));

  ctx->inSyntaxQuote = lastValue;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal trySymbolAnalyze(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error) {
  RetVal ret;

  wchar_t *sym = expr->symbol.value;
  ResolverBinding *resolved;

  if ((resolved = findResolverBinding(&ctx->resolverStack, sym)) != NULL) {
    form->type = F_ENV_REF;

    uint16_t currentTableIndex = getCurrentBindingTableIndex(ctx);

    if (resolved->tableIndex == currentTableIndex) {
      throws(tryEnvRefAnalyze(ctx, expr, resolved->bindingIndex, &form->envRef, error));
    }
    else {

      // first, look to see if we've already created a captured binding (they don't go in the resolver stack
      // because we discover them late)

      BindingTable *current = ctx->bindingTables.tables[currentTableIndex];
      for (uint16_t i=0; i<current->usedSpace; i++) {
        Binding *b = &current->bindings[i];

        if (b->source == BS_CAPTURED && wcscmp(b->name.value, sym) == 0) {
          throws(tryEnvRefAnalyze(ctx, expr, i, &form->envRef, error));
          break;
        }
      }

      /*
       * we need to plumb that binding through the intervening function definition boundaries
       *
       * iterate over the binding tables, starting with the one that defines the binding, and ending with the
       * current one.
       *
       * every function definition after the one that defines the binding needs to have a captured binding added,
       * referring to the binding from the previous definition
       */

      Binding captured = *resolved->binding;

      for (uint16_t i=resolved->tableIndex + 1; i<= currentTableIndex; i++) {
        BindingTable *this = ctx->bindingTables.tables[i];

        Binding binding;
        bindingInitContents(&binding);
        throws(tryTextCopy(ctx->pool, &captured.name, &binding.name, error));
        binding.source = BS_CAPTURED;
        binding.captured.bindingIndex = resolved->bindingIndex;

        addBinding(ctx->pool, this, binding);

        captured = binding;
      }

      throws(tryEnvRefAnalyze(ctx, expr, current->usedSpace - 1, &form->envRef, error));
    }
  }
  else {
    form->type = F_VAR_REF;
    throws(tryVarRefAnalyze(ctx, expr, &form->varRef, error));
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryExpandAnalyze(AnalyzerContext *ctx, Expr *expr, Form *form, Error *error) {
  RetVal ret;

  if (expr->type != N_LIST) {
    throwInternalError(error, "the contents of a macro argumet must be a list: %u", expr->type);
  }

  FormSymbol sym = expr->list.head->expr->symbol;

  Text macroName;
  macroName.length = sym.length;
  macroName.value = sym.value;

  Expr input;
  exprInitContents(&input);
  input.type = N_LIST;
  listInitContents(&input.list);
  input.list.length = expr->list.length - 1;
  input.list.head = expr->list.head->next;
  input.list.tail = expr->list.tail;

  Expr *output = NULL;
  throws(tryExpand(ctx->options.expander, macroName, &input, &output, error));
  throws(tryFormAnalyzeContents(ctx, output, form, error));

  failure:
  return ret;
}

RetVal tryListAnalyze(AnalyzerContext *ctx, Expr *expr, Form *form, Error *error) {
  RetVal ret;

  if (expr->type != N_LIST) {
    explode("not a list");
  }

  Form *fnCallable = NULL;
  {
    wchar_t hashMap[] = L"list";
    palloc(ctx->pool, fnCallable, sizeof(Form), "list callable");
    formInitContents(fnCallable);
    fnCallable->type = F_VAR_REF;
    throws(tryTextMake(ctx->pool, hashMap, &fnCallable->varRef.name, wcslen(hashMap), error));
  }

  Forms args;
  formsInitContents(&args);
  args.numForms = expr->list.length;
  palloc(ctx->pool, args.forms, sizeof(Form) * args.numForms, "Form array");

  ListElement *argExpr = expr->list.head;
  for (int i=0; argExpr != NULL; i++) {

    Form *arg = args.forms + i;
    formInitContents(arg);
    throws(tryFormAnalyzeContents(ctx, argExpr->expr, arg, error));

    argExpr = argExpr->next;
  }

  form->type = F_FN_CALL;
  fnCallInitContents(&form->fnCall);
  form->fnCall.fnCallable = fnCallable;
  form->fnCall.args = args;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryVecAnalyze(AnalyzerContext *ctx, Expr *expr, Form *form, Error *error) {
  RetVal ret;

  if (expr->type != N_VEC) {
    explode("not a vec");
  }

  Form *fnCallable = NULL;
  {
    wchar_t hashMap[] = L"vector";
    tryPalloc(ctx->pool, fnCallable, sizeof(Form), "vector callable");
    formInitContents(fnCallable);
    fnCallable->type = F_VAR_REF;
    throws(tryTextMake(ctx->pool, hashMap, &fnCallable->varRef.name, wcslen(hashMap), error));
  }

  Forms args;
  formsInitContents(&args);
  args.numForms = expr->vec.length;
  tryPalloc(ctx->pool, args.forms, sizeof(Form) * args.numForms, "Form array");

  ListElement *argExpr = expr->vec.head;
  for (int i=0; argExpr != NULL; i++) {

    Form *arg = args.forms + i;
    formInitContents(arg);
    throws(tryFormAnalyzeContents(ctx, argExpr->expr, arg, error));

    argExpr = argExpr->next;
  }

  form->type = F_FN_CALL;
  fnCallInitContents(&form->fnCall);
  form->fnCall.fnCallable = fnCallable;
  form->fnCall.args = args;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryMapAnalyze(AnalyzerContext *ctx, Expr *expr, Form *form, Error *error) {
  RetVal ret;

  if (expr->type != N_MAP) {
    explode("not a map");
  }

  Form *fnCallable = NULL;
  {
    wchar_t hashMap[] = L"hash-map";
    tryPalloc(ctx->pool, fnCallable, sizeof(Form), "hashmap callable");
    formInitContents(fnCallable);
    fnCallable->type = F_VAR_REF;
    throws(tryTextMake(ctx->pool, hashMap, &fnCallable->varRef.name, wcslen(hashMap), error));
  }

  Forms args;
  formsInitContents(&args);
  args.numForms = expr->map.length * 2;
  tryPalloc(ctx->pool, args.forms, sizeof(Form) * args.numForms, "Form array");

  MapElement *argExpr = expr->map.head;
  for (int i=0; argExpr != NULL; i+=2) {

    Form *keyArg = args.forms + i;
    formInitContents(keyArg);
    throws(tryFormAnalyzeContents(ctx, argExpr->key, keyArg, error));

    Form *valArg = args.forms + i + 1;
    formInitContents(valArg);
    throws(tryFormAnalyzeContents(ctx, argExpr->value, valArg, error));

    argExpr = argExpr->next;
  }

  form->type = F_FN_CALL;
  fnCallInitContents(&form->fnCall);
  form->fnCall.fnCallable = fnCallable;
  form->fnCall.args = args;

  return R_SUCCESS;

  failure:
  return ret;
}

void _handlerInitContents(FormHandler *h) {
  h->handler = NULL;
  formsInitContents(&h->forms);
}

RetVal tryHandlerAnalyze(AnalyzerContext *ctx, Expr* expr, FormHandler *handler, Error *error) {
  RetVal ret;

  _handlerInitContents(handler);

  uint64_t pos = getExprPosition(expr);
  if (expr->list.length < 1) {
    throwSyntaxError(error, pos, "the 'with-handler' special form requires at least one parameter");
  }

  palloc(ctx->pool, handler->handler, sizeof(Form), "Form");
  throws(tryFormAnalyzeContents(ctx, expr->list.head->next->expr, handler->handler, error));

  formsAllocate(ctx->pool, &handler->forms, expr->list.length - 2);

  ListElement *exprElem = expr->list.head->next->next;
  for (int i=0; i<handler->forms.numForms; i++) {
    Form *thisForm = handler->forms.forms + i;
    throws(tryFormAnalyzeContents(ctx, exprElem->expr, thisForm, error));
    exprElem = exprElem->next;
  }

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryFormAnalyzeContents(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error) {

  // copy expression source metadata
  form->source = expr->source;

  RetVal ret;

  switch (expr->type) {

    // always-constants
    case N_STRING:
      form->type = F_CONST;
      form->constant = stringMake(ctx->pool, expr->string.value, expr->string.length);
      break;
    case N_NUMBER:
      form->type = F_CONST;
      form->constant = numberMake(ctx->pool, expr->number.value);
      break;
    case N_CHAR:
      form->type = F_CONST;
      form->constant = charMake(ctx->pool, expr->chr.value);
      break;
    case N_KEYWORD:
      form->type = F_CONST;
      form->constant = keywordMake(ctx->pool, expr->keyword.value, expr->keyword.length);
      break;
    case N_BOOLEAN:
      form->type = F_CONST;
      form->constant = booleanMake(ctx->pool, expr->boolean.value);
      break;
    case N_NIL:
      form->type = F_CONST;
      form->constant = nilMake(ctx->pool);
      break;

    case N_VEC: {
      throws(tryVecAnalyze(ctx, expr, form, error));
      break;
    }

    case N_MAP: {
      throws(tryMapAnalyze(ctx, expr, form, error));
      break;
    }

    case N_SYMBOL: {
      if (ctx->inQuote) {
        form->type = F_CONST;
        // TODO: this should namespace the symbol, but for that we'd have to know what the current namespace is
        form->constant = symbolMake(ctx->pool, expr->symbol.value, expr->symbol.length);
      }
      else if (ctx->inSyntaxQuote) {
        form->type = F_CONST;
        // TODO: this should namespace the symbol, but for that we'd have to know what the current namespace is
        form->constant = symbolMake(ctx->pool, expr->symbol.value, expr->symbol.length);
      }
      else {
        throws(trySymbolAnalyze(ctx, expr, form, error));
      }
      break;
    }

    case N_LIST: {

      if (expr->list.length == 0 || ctx->inQuote) {
        throws(tryListAnalyze(ctx, expr, form, error));
        break;
      }

      if (ctx->inSyntaxQuote) {
        Expr *quoted = expr;

        bool unquoted =
            quoted->list.length > 0
            && quoted->list.head->expr->type == N_SYMBOL
            && wcscmp(quoted->list.head->expr->symbol.value, L"unquote") == 0;

        if (unquoted) {
          if (quoted->list.length != 2) {
            throwSyntaxError(error, quoted->source.position, "unquote takes 1 argument");
          }
          bool last = ctx->inSyntaxQuote;
          ctx->inSyntaxQuote = false;
          throws(tryFormAnalyzeContents(ctx, quoted->list.head->next->expr, form, error));
          ctx->inSyntaxQuote = last;
        }
        else {
          trySyntaxQuoteListAnalyze(ctx, quoted, form, error);
        }

        break;
      }

      // special forms
      if (expr->list.head->expr->type == N_SYMBOL) {
        wchar_t *sym = expr->list.head->expr->symbol.value;

        if (wcscmp(sym, L"if") == 0) {
          form->type = F_IF;
          throws(tryIfAnalyze(ctx, expr, &form->iff, error));
          break;
        }

        if (wcscmp(sym, L"let*") == 0) {
          form->type = F_LET;
          throws(tryLetAnalyze(ctx, expr, &form->let, error));
          break;
        }

        if (wcscmp(sym, L"def") == 0) {
          form->type = F_DEF;
          throws(tryDefAnalyze(ctx, expr, &form->def, error));
          break;
        }

        if (wcscmp(sym, L"fn") == 0) {
          form->type = F_FN;
          throws(tryFnAnalyze(ctx, expr, &form->fn, error));
          break;
        }

        if (wcscmp(sym, L"quote") == 0) {
          form->type = F_CONST;
          throws(tryQuoteAnalyze(ctx, expr, form, error));
          form->source = form->constant->source;
          break;
        }

        if (wcscmp(sym, L"syntax-quote") == 0) {
          throws(trySyntaxQuoteAnalyze(ctx, expr, form, error));
          break;
        }

        if (wcscmp(sym, L"builtin") == 0) {
          form->type = F_BUILTIN;
          throws(tryBuiltinAnalyze(ctx, expr, &form->builtin, error));
          break;
        }

        if (wcscmp(sym, L"with-handler") == 0) {
          form->type = F_HANDLER;
          throws(tryHandlerAnalyze(ctx, expr, &form->handler, error));
          break;
        }

        /*
         * macro expansion
         */
        if (ctx->options.expander != NULL) {
          Text text;
          text.length = wcslen(sym);
          text.value = sym;
          bool isMacro;
          throws(tryIsMacro(ctx->options.expander, text, &isMacro, error));
          if (isMacro) {
            throws(tryExpandAnalyze(ctx, expr, form, error));
            break;
          }
        }
      }

      // assume fn-callable
      form->type = F_FN_CALL;
      throws(tryFnCallAnalyze(ctx, expr, &form->fnCall, error));
      break;
    }

    default: {
      throwInternalError(error, "unhandled expression type: '%i'", expr->type);
    }
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal _tryFormAnalyze(AnalyzerContext *ctx, Expr* expr, Form **ptr, Error *error) {

  RetVal ret;

  Form *form; // clean up on fail

  tryPalloc(ctx->pool, form, sizeof(Form), "Form");
  formInitContents(form);
  throws(tryFormAnalyzeContents(ctx, expr, form, error));

  *ptr = form;
  return R_SUCCESS;

  failure:
  return ret;
}

void rootInitContents(FormRoot *root) {
  bindingTableInitContents(&root->table);
  root->form = NULL;
  textInitContents(&root->fileName);
  root->hasFileName = false;
}

void analyzerContextInitContents(AnalyzerContext *ctx) {
  ctx->pool = NULL;
  bindingTablesInitContents(&ctx->bindingTables);
  resolverStackInitContents(&ctx->resolverStack);
  analyzeOptionsInitContents(&ctx->options);
  ctx->inQuote = false;
  ctx->inSyntaxQuote = false;
}

void analyzeOptionsInitContents(AnalyzeOptions *options) {
  options->expander = NULL;
  options->hasFileName = false;
  textInitContents(&options->fileName);
}

RetVal tryFormAnalyzeOptions(AnalyzeOptions options, Expr* expr, Pool_t pool, FormRoot **ptr, Error *error) {
  RetVal ret;
  AnalyzerContext ctx;

  analyzerContextInitContents(&ctx);
  ctx.options = options;
  ctx.pool = pool;

  FormRoot *root = NULL;
  palloc(pool, root, sizeof(FormRoot), "FormRoot");
  rootInitContents(root);
  pushBindingTable(ctx.pool, &ctx.bindingTables, &root->table);

  throws(_tryFormAnalyze(&ctx, expr, &root->form, error));

  popBindingTable(&ctx.bindingTables);

  if (options.hasFileName) {
    root->hasFileName = true;
    throws(tryTextCopy(pool, &options.fileName, &root->fileName, error));
  }

  *ptr = root;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryFormAnalyze(Expr* expr, Pool_t pool, FormRoot **ptr, Error *error) {
  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);
  return tryFormAnalyzeOptions(options, expr, pool, ptr, error);
}


