#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "analyzer.h"
#include "utils.h"
#include "expander.h"

/*
 * TODO: is this env binding within my local scope, or am I capturing this from a parent scope?
 *
 * you might know this by examining the binding scopes, working backwards until you get to one defined as part of a function definition
 * if you can't resolve this env ref without crossing that boundary, you know you're capturing
 *
 * if you're capturing, then you need to somehow indicate that so the compiler knows to include this captured value
 * as a part of the current closure
 * - you need to identify which value is being captured (by unique binding id)
 *
 * TODO: within a single Form tree, all bindings must have a unique id
 *
 * the compiler will need to identify the need to create closures based on aggregating these captured values for each fn definition
 * the compiler will need to then compute local indexes based on these plus the fn args and all the normally bound locals wthin a function
 *
 *
 *
 * bindings can be defined by
 * - a fn
 * - a let
 *
 * referenced bindings originating from outside a fn definition must be captured as a part of a closure
 *
 * // TODO: bindings must be defined in the Form tree homogenously, so they can be collected by the compiler
 *    and compared to the references that refer to them. This way the compiler can emit code to create closures
 *    as needed.
 *
 * // TODO: frame locals should not be computed based on references, but based on bindings
 */

/*
 * More from notes:
 *
 *
 * Each fn and the root expr get their own binding tables. These tables can be appended to as captured bindings and
 * let bindings are discovered. References within a function use the binding typeIndex to identify them.
 *
 * Also the idea of a stack frame node specifically to represent a top-level expression separate from the idea of a
 * function definition. The function would be additive.
 */

/*
 * (let (a 100)
 *   (fn X ()
 *     (let (b 200)
 *       (fn Y ()      -- we need to emit a closure here, we need to load a and b from their locals
 *         (+ a b))))) -- but a and b are defined as locals in different stack frames
 *                     -- which means a becomes an extra parameter to X, and a and b become extra parameters to Y
 *
 *                     -- the behavior needed is to seek up the binding stack until a can be resolved. each function
 *                     -- boundary that is crossed to resolve a must have a added to the list of bindings that function
 *                     -- captures. this causes the compiler to allocate local space for a in each function's stack frame,
 *                     -- as well as to create the function as a closure to populate the local space
 *
 *                     -- this way, each captured binding (a and b) are resolved into locals within Y
 *
 * The Plan
 * - the analyzer needs a form type to describe lexical bindings uniformly
 *
 * - the analyzer needs to use a single binding stack for an entire analysis run, no longer creating new binding stacks for fn's
 *   - the analyzer needs to drop the idea of 'scopes', and use a simple array stack where each item in the stack is a union of:
 *     - a let binding (includes unique let typeIndex within a function)
 *     - a function reference binding (only one defined per function)
 *     - a function arg binding (includes arg typeIndex within a function)
 *     - a function definition boundary, referencing a FormFn
 *     instead of pushing/popping scopes, the analyzer can save the current stack height, and then pop back down to it
 *     efficiently when it is done with a particular binding scope
 *
 * - the analyzer needs to walk through the binding stack to resolve binding references as it encounters them. it
 *   starts at the top of the stack and keeps moving down until it finds a binding with a matching name, or hits the
 *   bottom of the stack and errors out because the binding reference cannot be resolved
 *
 * - the analyzer must emit a FormEnvRef that identifies which binding it references. bindings are identified by type
 *   and typeIndex. FormEnvRefs can only identify bindings that originate from within a function or top-level code unit.
 *   All indexes are function and ref-type specific. (types are 'fn-capture', 'fn-self-reference', 'fn-arg', 'let')
 *
 * - if a binding reference is resolved, and it originates from outside a function (it is captured), then each
 *   function definition boundary crossed must have the binding added to it as a captured binding.
 *
 * - captured bindings explicitly reference a binding defined in the function definition boundary immediately above
 *   where the function is defined. otherwise, they are identical to regular binding references.
 *
 * - the compiler needs to build a binding lookup table for all bindings in the CodeUnit and all declared functions,
 *   organizing bindings by id.
 *
 * - the compiler, for each code block, (for code units and fn constants)
 *
 *   // TODO it would be nice if the analyzer gave all the bindings a unique id within a function up-front to make this easier
 *   // it would use this id for all the references
 *
 *   - assigns all the bindings defined a storage location from which they can be loaded when
 *     referenced. these storage locations are stored in a binding table
 *     - function-name bindings have values that are stored as constants
 *     - let-bindings have values that are stored as locals
 *
 *   - when the compiler encounters a binding reference
 *     - if the binding reference refers to a function by name, create a fn-ref-const and emit a LOAD_CONST
 *     - if the binding reference refers to a binding created within the local code scope, emit a LOAD_LOCAL. locals
 *       are referenced by typeIndex. these indexes must be computed
 *
 *   - the compiler should traverse
 *
 *   - the compiler should collect a list of all the captured bindings
 *     if there are one or more captures, the function declaration should be wrapped in a call to the vm to create a closure
 *
 *   - the compiler should build a locals table, and assign locally-defined bindings into it
 *
 *   - the compiler should emit LOADS and STOREs for locals based on the values in this table
 *   - the compiler should emit a LOAD_CONST wherever function references are encountered
 */


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
  AnalyzeOptions options;
  BindingTables bindingTables;
  ResolverStack resolverStack;
  uint16_t fnCount;
} AnalyzerContext;


void bindingInitContents(Binding *binding) {
  textInitContents(&binding->name);
  binding->source = BS_NONE;
}

void bindingFreeContents(Binding *binding) {
  if (binding != NULL) {
    textFreeContents(&binding->name);
    binding->source = BS_NONE;
  }
}

void bindingTableInitContents(BindingTable *table) {
  table->bindings = NULL;
  table->usedSpace = 0;
  table->allocatedSpace = 0;
}

void bindingTableFreeContents(BindingTable *table) {
  if (table != NULL) {
    table->usedSpace = 0;
    table->allocatedSpace = 0;
    if (table->bindings != NULL) {
      free(table->bindings);
      table->bindings = NULL;
    }
  }
}

void bindingTablesInitContents(BindingTables *tables) {
  tables->usedSpace = 0;
  tables->allocatedSpace = 0;
  tables->tables = NULL;
}

void bindingTablesFreeContents(BindingTables *tables) {
  if (tables != NULL) {
    tables->allocatedSpace = 0;
    tables->usedSpace = 0;
    if (tables->tables != NULL) {
      free(tables->tables);
      tables->tables = NULL;
    }
  }
}

RetVal tryAddBinding(BindingTable *table, Binding binding, Error *error) {
  RetVal ret;

  if (table->bindings == NULL) {
    uint16_t len = 16;
    tryMalloc(table->bindings, len * sizeof(Binding), "Binding array");
    table->allocatedSpace = len;
  }
  else if (table->usedSpace == table->allocatedSpace) {
    uint64_t newAllocatedLength = table->allocatedSpace * 2;

    Binding *resized = realloc(table->bindings, newAllocatedLength * sizeof(Binding));
    if (resized == NULL) {
      ret = memoryError(error, "realloc Binding array");
      goto failure;
    }

    table->allocatedSpace = newAllocatedLength;
    table->bindings = resized;
  }

  uint64_t index = table->usedSpace;
  table->bindings[index] = binding;
  table->usedSpace = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryPushBindingTable(BindingTables *tables, BindingTable *table, Error *error) {
  RetVal ret;

  if (tables->tables == NULL) {
    uint16_t len = 16;
    tryMalloc(tables->tables, len * sizeof(BindingTable*), "BindingTable pointer array");
    tables->allocatedSpace = len;
  }
  else if (tables->usedSpace == tables->allocatedSpace) {
    uint64_t newAllocatedLength = tables->allocatedSpace * 2;

    BindingTable** resizedTables = realloc(tables->tables, newAllocatedLength * sizeof(BindingTable*));
    if (resizedTables == NULL) {
      ret = memoryError(error, "realloc BindingTable pointer array");
      goto failure;
    }

    tables->allocatedSpace = newAllocatedLength;
    tables->tables = resizedTables;
  }

  uint64_t index = tables->usedSpace;
  tables->tables[index] = table;
  tables->usedSpace = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryPopBindingTable(BindingTables *tables, Error *error) {
  RetVal ret;

  if (tables->usedSpace == 0) {
    throwInternalError(error, "cannot pop binding table from empty stack");
  }

  tables->tables[tables->usedSpace - 1] = NULL;
  tables->usedSpace = tables->usedSpace - 1;

  return R_SUCCESS;

  failure:
  return ret;
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

void resolverStackFreeContents(ResolverStack *stack) {
  if (stack != NULL) {
    stack->usedSpace = 0;
    stack->allocatedSpace = 0;
    if (stack->bindings != NULL) {
      free(stack->bindings);
      stack->bindings = NULL;
    }
  }
}

RetVal tryPushResolverBinding(ResolverStack *stack, ResolverBinding binding, Error *error) {
  RetVal ret;

  if (stack->bindings == NULL) {
    uint16_t len = 16;
    tryMalloc(stack->bindings, len * sizeof(ResolverBinding), "ResolverBinding array");
    stack->allocatedSpace = len;
  }
  else if (stack->usedSpace == stack->allocatedSpace) {
    uint64_t newAllocatedLength = stack->allocatedSpace * 2;

    ResolverBinding* resized = realloc(stack->bindings, newAllocatedLength * sizeof(ResolverBinding));
    if (resized == NULL) {
      ret = memoryError(error, "realloc ResolverBinding array");
      goto failure;
    }

    stack->allocatedSpace = newAllocatedLength;
    stack->bindings = resized;
  }

  uint64_t index = stack->usedSpace;
  stack->bindings[index] = binding;
  stack->usedSpace = index + 1;

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryPopResolverBindings(ResolverStack *stack, uint16_t numBindings, Error *error) {
  RetVal ret;

  if (stack->usedSpace < numBindings) {
    throwInternalError(error, "cannot pop more bindings than exist");
  }

  stack->usedSpace = stack->usedSpace - numBindings;
  return R_SUCCESS;

  failure:
  return ret;
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

RetVal tryGetCurrentBindingTableIndex(AnalyzerContext *ctx, uint16_t *idx, Error *error) {
  RetVal ret;

  if (ctx->bindingTables.usedSpace == 0) {
    throwInternalError(error, "no current binding table found");
  }

  *idx = ctx->bindingTables.usedSpace - 1;
  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryCreateBinding(AnalyzerContext *ctx, Binding binding, uint16_t *bindingIndexPtr, Error *error) {
  RetVal ret;

  if (ctx->bindingTables.usedSpace == 0) {
    throwInternalError(error, "no current binding table found");
  }

  uint16_t tableIndex = ctx->bindingTables.usedSpace - 1;
  BindingTable *table = ctx->bindingTables.tables[tableIndex];
  throws(tryAddBinding(table, binding, error));
  uint16_t bindingIndex = table->usedSpace - 1;
  Binding *b = &table->bindings[bindingIndex];

  ResolverBinding resolver;
  resolverBindingInitContents(&resolver);
  resolver.tableIndex = tableIndex;
  resolver.bindingIndex = bindingIndex;
  resolver.binding = b;

  throws(tryPushResolverBinding(&ctx->resolverStack, resolver, error));

  *bindingIndexPtr = bindingIndex;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryPopBindings(AnalyzerContext *ctx, uint16_t numBindings, Error *error) {
  RetVal ret;

  throws(tryPopResolverBindings(&ctx->resolverStack, numBindings, error));
  return R_SUCCESS;

  failure:
  return ret;
}

//RetVal addFnBinding(EnvBindingStack *stack, FormFn *fn, Error *error) {
//  RetVal ret;
//
//  EnvBinding e;
//  e.nameLength = fn->nameLength;
//  throws(tryCopyText(fn->name, &e.name, e.nameLength, error));
//  e.type = RT_FN;
//  e.index = fn->id;
//
//  throws(addEnvBinding(stack, e, BC_FN, error));
//
//  return R_SUCCESS;
//
//  failure:
//  return ret;
//}

/*
 * Analyzers for the different types of forms.
 */

RetVal _tryFormAnalyze(AnalyzerContext *ctx, Expr* expr, Form **ptr, Error *error);
RetVal tryFormAnalyzeContents(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error);
void formFreeContents(Form* form);
void formFree(Form* form);

uint64_t getExprPosition(Expr *expr) {
  return expr->source.position;
}

uint64_t getFormPosition(Form *form) {
  return form->source.position;
}

void ifFreeContents(FormIf *iff);

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
  Expr *elseBranchExpr = ifExpr->list.head->next->next->next->expr;

  throws(_tryFormAnalyze(ctx, testExpr, &iff->test, error));
  throws(_tryFormAnalyze(ctx, ifBranchExpr, &iff->ifBranch, error));
  throws(_tryFormAnalyze(ctx, elseBranchExpr, &iff->elseBranch, error));

  return R_SUCCESS;

  failure:
    ifFreeContents(iff);
    return ret;
}

void ifFreeContents(FormIf *iff) {
  if (iff != NULL) {
    if (iff->test != NULL) {
      formFree(iff->test);
      iff->test = NULL;
    }
    if (iff->ifBranch != NULL) {
      formFree(iff->ifBranch);
      iff->ifBranch = NULL;
    }
    if (iff->elseBranch != NULL) {
      formFree(iff->elseBranch);
      iff->elseBranch = NULL;
    }
  }
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

RetVal tryFormsAllocate(Forms *forms, uint16_t length, Error *error) {
  RetVal ret;

  forms->numForms = length;
  tryMalloc(forms->forms, sizeof(Form) * forms->numForms, "Forms array");

  return R_SUCCESS;

  failure:
    return ret;
}

void formsFreeContents(Forms *forms) {
  if (forms != NULL) {
    if (forms->forms != NULL) {
      for (uint16_t i=0; i<forms->numForms; i++) {
        formFreeContents(&forms->forms[i]);
      }
      forms->numForms = 0;
      free(forms->forms);
      forms->forms = NULL;
    }
  }
}

void letInitContents(FormLet *let) {
  let->numBindings = 0;
  let->bindings = NULL;
  formsInitContents(&let->forms);
}

void letBindingInitContents(LetBinding *b) {
  textInitContents(&b->name);
  sourceLocationInitContents(&b->source);
  b->value = NULL;
}

void letBindingFreeContents(LetBinding *b) {
  if (b != NULL) {
    textFreeContents(&b->name);
    sourceLocationFreeContents(&b->source);
    if (b->value != NULL) {
      formFree(b->value);
      b->value = NULL;
    }
  }
}

void letFreeContents(FormLet *let);

RetVal tryLetAnalyze(AnalyzerContext *ctx, Expr* letExpr, FormLet *let, Error *error) {
  RetVal ret;

  uint16_t numBindingsPushed = 0;

  letInitContents(let);

  // sanity checking
  uint64_t pos = getExprPosition(letExpr);
  if (letExpr->list.length < 2) {
    throwSyntaxError(error, pos, "the 'let' special form requires at least one parameter");
  }
  Expr *bindingsExpr = letExpr->list.head->next->expr;
  if (bindingsExpr->type != N_LIST) {
    throwSyntaxError(error, pos, "the 'let' special form requires the first parameter to be a list");
  }
  if (bindingsExpr->list.length % 2 != 0) {
    throwSyntaxError(error, pos, "the 'let' special form requires the first parameter to be a list with an even number of arguments");
  }

  // create the bindings
  let->numBindings = bindingsExpr->list.length / 2;
  tryMalloc(let->bindings, sizeof(LetBinding) * let->numBindings, "LetBinding array");

  // add the bindings to the current binding table
  // push the bindings on the current binding stack

  // initialize the bindings
  ListElement *bindingElem = bindingsExpr->list.head;
  for (int i=0; bindingElem != NULL; i++) {

    if (bindingElem->expr->type != N_SYMBOL) {
      throwSyntaxError(error, pos, "only symbols can be bound as names");
    }

    LetBinding *b = let->bindings + i;
    letBindingInitContents(b);
    throws(tryTextMake(bindingElem->expr->symbol.value, &b->name, bindingElem->expr->symbol.length, error));
    b->source = bindingElem->expr->source;
    throws(_tryFormAnalyze(ctx, bindingElem->next->expr, &b->value, error));

    Binding binding;
    bindingInitContents(&binding);
    throws(tryTextCopy(&b->name, &binding.name, error));
    binding.source = BS_LOCAL;
    binding.local.type = BT_LET;
    binding.local.typeIndex = i;

    throws(tryCreateBinding(ctx, binding, &b->bindingIndex, error));

    bindingElem = bindingElem->next->next;
  }

  // create the forms within this lexical scope
  throws(tryFormsAllocate(&let->forms, letExpr->list.length - 2, error));

  ListElement *exprElem = letExpr->list.head->next->next;
  for (int i=0; i<let->forms.numForms; i++) {
    Form *thisForm = let->forms.forms + i;
    throws(tryFormAnalyzeContents(ctx, exprElem->expr, thisForm, error));
    exprElem = exprElem->next;
  }

  // discard the registered bindings from the environment stack
  numBindingsPushed = 0;
  throws(tryPopBindings(ctx, let->numBindings, error));

  return R_SUCCESS;

  failure:
    letFreeContents(let);
    throws(tryPopBindings(ctx, numBindingsPushed, error))
    return ret;
}

void letFreeContents(FormLet *let) {
  if (let != NULL) {
    if (let->bindings != NULL) {
      for (int i=0; i<let->numBindings; i++) {
        LetBinding *b = let->bindings + i;
        letBindingFreeContents(b);
      }
      free(let->bindings);
      let->bindings = NULL;
      let->numBindings = 0;
    }
    formsFreeContents(&let->forms);
  }
}

void defInitContents(FormDef *def) {
  textInitContents(&def->name);
  def->value = NULL;
}

void defFreeContents(FormDef *def);

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

  throws(tryTextMake(symbol->symbol.value, &def->name, symbol->symbol.length, error));

  if (defExpr->list.length == 3) {
    throws(_tryFormAnalyze(ctx, defExpr->list.head->next->next->expr, &def->value, error));
  }

  // update the analyzer state so it knows about this def
  //  throws(tryDefVar(analyzer, def->name, def->nameLength, def->value, error));

  return R_SUCCESS;

  failure:
    defFreeContents(def);
    return ret;
}

void defFreeContents(FormDef *def) {
  if (def != NULL) {
    textFreeContents(&def->name);
    if (def->value != NULL) {
      formFree(def->value);
      def->value = NULL;
    };
  }
}

void fnFreeContents(FormFn *fn);


void formFnInitContents(FormFn *fn) {
  bindingTableInitContents(&fn->table);
  fn->hasName = false;
  textInitContents(&fn->name);
  fn->id = 0;
  fn->args = NULL;
  fn->numArgs = 0;
  formsInitContents(&fn->forms);
}

void fnArgInitContents(FormFnArg *arg) {
  textInitContents(&arg->name);
  sourceLocationInitContents(&arg->source);
}

void fnArgFreeContents(FormFnArg *arg) {
  if (arg != NULL) {
    textFreeContents(&arg->name);
    sourceLocationFreeContents(&arg->source);
  }
}

RetVal tryFnParse(Expr* fnExpr, FormFn *fn, Expr **formElements, Error *error) {
  RetVal ret;

  uint64_t pos = getExprPosition(fnExpr);
  ListElement *itr = fnExpr->list.head->next;

  if (itr == NULL) {
    throwSyntaxError(error, pos, "the 'fn' special form requires at least one parameter");
  }

  // the optional function name
  if (itr->expr->type == N_SYMBOL) {
    throws(tryTextMake(itr->expr->symbol.value, &fn->name, itr->expr->symbol.length, error));
    fn->hasName = true;
    itr = itr->next;
  }

  // create the arguments
  if (itr == NULL) {
    throwSyntaxError(error, pos, "the 'fn' special form requires an argument list");
  }

  Expr *argsExpr = itr->expr;
  if (argsExpr->type != N_LIST) {
    throwSyntaxError(error, pos, "the 'fn' special form requires an argument list of the type N_LIST: %u",
                     argsExpr->type);
  }

  fn->numArgs = argsExpr->list.length;
  tryMalloc(fn->args, sizeof(FormFnArg) * fn->numArgs, "FormFnArg array");

  ListElement *argElem = argsExpr->list.head;
  for (int i=0; i<fn->numArgs; i++) {

    if (argElem->expr->type != N_SYMBOL) {
      throwSyntaxError(error, pos, "only symbols can be used as function arguments");
    }

    FormFnArg *arg = fn->args + i;
    fnArgInitContents(arg);
    throws(tryTextMake(argElem->expr->symbol.value, &arg->name, argElem->expr->symbol.length, error));
    arg->source = argElem->expr->source;

    argElem = argElem->next;
  }
  itr = itr->next;

  uint16_t nonFormElems = 2; // 'fn' and 'args list'
  if (fn->hasName) {
    nonFormElems = nonFormElems + 1; // optional function name
  }

  fn->forms.numForms = fnExpr->list.length - nonFormElems;
  tryMalloc(*formElements, sizeof(Expr) * fn->forms.numForms, "Expr array");

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
      _markTailCalls(last->iff.elseBranch);
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
  throws(tryFnParse(fnExpr, fn, &formElements, error));

  fn->id = ctx->fnCount;
  ctx->fnCount = ctx->fnCount + 1;

  // create new binding stack, initialized with the fn args as the first bindings

  throws(tryPushBindingTable(&ctx->bindingTables, &fn->table, error));
  uint16_t numBindingsPushed = 0;

  if (fn->hasName) {

    Binding binding;
    bindingInitContents(&binding);
    throws(tryTextCopy(&fn->name, &binding.name, error));
    binding.source = BS_LOCAL;
    binding.local.type = BT_FN_REF;
    binding.local.typeIndex = 0;

    throws(tryCreateBinding(ctx, binding, &fn->bindingIndex, error));
    numBindingsPushed = numBindingsPushed + 1;
  }

  for (uint64_t i=0; i<fn->numArgs; i++) {

    Binding binding;
    bindingInitContents(&binding);
    throws(tryTextCopy(&fn->args[i].name, &binding.name, error));
    binding.source = BS_LOCAL;
    binding.local.type = BT_FN_ARG;
    binding.local.typeIndex = i;

    throws(tryCreateBinding(ctx, binding, &fn->args[i].bindingIndex, error));
    numBindingsPushed = numBindingsPushed + 1;
  }

  // create the forms within this fn lexical scope
  throws(tryFormsAllocate(&fn->forms, fn->forms.numForms, error));
  for (int i=0; i<fn->forms.numForms; i++) {
    Expr expr = formElements[i];
    Form *thisForm = fn->forms.forms + i;
    throws(tryFormAnalyzeContents(ctx, &expr, thisForm, error));
  }

  markTailCalls(fn);

  fn->numCaptures = bindingTableCaptures(&fn->table);
  fn->isClosure = fn->numCaptures > 0;

  throws(tryPopBindings(ctx, numBindingsPushed, error));
  throws(tryPopBindingTable(&ctx->bindingTables, error));

  ret = R_SUCCESS;
  goto done;

  failure:
    fnFreeContents(fn);
    throws(tryPopBindings(ctx, numBindingsPushed, error));
    throws(tryPopBindingTable(&ctx->bindingTables, error));
    goto done;

  done:
    if (formElements != NULL) {
      free(formElements);
    }
    return ret;
}


void fnFreeContents(FormFn *fn) {
  if (fn != NULL) {
    bindingTableFreeContents(&fn->table);
    textFreeContents(&fn->name);
    if (fn->args != NULL) {
      for (int i=0; i<fn->numArgs; i++) {
        FormFnArg *arg = fn->args + i;
        fnArgFreeContents(arg);
      }
      free(fn->args);
      fn->args = NULL;
      fn->numArgs = 0;
    }
    formsFreeContents(&fn->forms);
  }
}

void builtinInitContents(FormBuiltin *builtin) {
  textInitContents(&builtin->name);
  formsInitContents(&builtin->args);
}

void builtinFreeContents(FormBuiltin *builtin);

RetVal tryBuiltinAnalyze(AnalyzerContext *ctx, Expr *expr, FormBuiltin *builtin, Error *error) {

  RetVal ret;

  builtinInitContents(builtin);

  Expr *name = expr->list.head->next->expr;
  if (name->type != N_KEYWORD) {
    throwSyntaxError(error, name->source.position, "the 'builtin' special form requires the first parameter to be a keyword");
  }

  throws(tryTextMake(name->keyword.value, &builtin->name, name->keyword.length, error));

  throws(tryFormsAllocate(&builtin->args, expr->list.length - 2, error));

  ListElement *argExpr = expr->list.head->next->next;
  for (int i=0; i<builtin->args.numForms; i++) {
    Form *arg = builtin->args.forms + i;
    throws(tryFormAnalyzeContents(ctx, argExpr->expr, arg, error));
    argExpr = argExpr->next;
  }

  return R_SUCCESS;

  failure:
    builtinFreeContents(builtin);
    return ret;
}

void builtinFreeContents(FormBuiltin *builtin) {
  if (builtin != NULL) {
    textFreeContents(&builtin->name);
    formsFreeContents(&builtin->args);
  }
}

RetVal tryEnvRefAnalyze(AnalyzerContext *ctx, Expr *expr, uint16_t bindingIndex, FormEnvRef *envRef, Error *error) {
  envRef->bindingIndex = bindingIndex;
  return R_SUCCESS;
}

void envRefFreeContents(FormEnvRef *ref) {
  // nothing to do
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

  throws(tryTextMake(expr->symbol.value, &varRef->name, expr->symbol.length, error));

  return R_SUCCESS;

  failure:
    return ret;
}

void varRefFreeContents(FormVarRef *ref) {
  if (ref != NULL) {
    textFreeContents(&ref->name);
  }
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
}

void fnCallFreeContents(FormFnCall *fnCall);

RetVal tryFnCallAnalyze(AnalyzerContext *ctx, Expr *expr, FormFnCall *fnCall, Error *error) {

  RetVal ret;

  fnCallInitContents(fnCall);

  throws(_tryFormAnalyze(ctx, expr->list.head->expr, &fnCall->fnCallable, error));
  throws(assertFnCallable(fnCall->fnCallable, error));
  throws(tryFormsAllocate(&fnCall->args, expr->list.length - 1, error));

  ListElement *argExpr = expr->list.head->next;
  for (int i=0; i<fnCall->args.numForms; i++) {
    Form *arg = fnCall->args.forms + i;
    throws(tryFormAnalyzeContents(ctx, argExpr->expr, arg, error));
    argExpr = argExpr->next;
  }

  return R_SUCCESS;

  failure:
    fnCallFreeContents(fnCall);
    return ret;
}

void fnCallFreeContents(FormFnCall *fnCall) {
  if (fnCall != NULL) {
    if (fnCall->fnCallable != NULL) {
      formFree(fnCall->fnCallable);
      fnCall->fnCallable = NULL;
    }
    formsFreeContents(&fnCall->args);
  }
}

RetVal tryConstantAnalyze(Expr* expr, Expr **constant, Error *error) {

  RetVal ret;

  throws(tryExprDeepCopy(expr, constant, error));
  return R_SUCCESS;

  failure:
    return ret;
}

void constantFreeContents(Expr *constant) {
  if (constant != NULL) {
    exprFree(constant);
  }
}

RetVal tryQuoteAnalyze(Expr* expr, Expr **constant, Error *error) {

  RetVal ret;

  if (expr->list.length != 2) {
    throwSyntaxError(error, expr->source.position, "wrong number of args passed to quote (%llu instead of 1)",
                     expr->list.length - 1);
  }

  throws(tryConstantAnalyze(expr->list.head->next->expr, constant, error));
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

    uint16_t currentTableIndex;
    throws(tryGetCurrentBindingTableIndex(ctx, &currentTableIndex, error));

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
        throws(tryTextCopy(&captured.name, &binding.name, error));
        binding.source = BS_CAPTURED;
        binding.captured.bindingIndex = resolved->bindingIndex;

        throws(tryAddBinding(this, binding, error));

        captured = binding;
      }

      throws(tryEnvRefAnalyze(ctx, expr, current->usedSpace - 1, &form->envRef, error));
    }
  }
  else { // if ((var = resolveVar(analyzer, sym, expr->symbol.length)) != NULL) {
    form->type = F_VAR_REF;
    throws(tryVarRefAnalyze(ctx, expr, &form->varRef, error));
  }
//      else {
//        throwSyntaxError(error, getExprPosition(expr), "cannot resolve symbol: '%ls'", sym);
//      }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryExpandAnalyze(AnalyzerContext *ctx, Expr *expr, Form *form, Error *error) {
  RetVal ret;

  if (expr->type != VT_LIST) {
    throwInternalError(error, "the contents of a macro argumet must be a list: %u", expr->type);
  }

  ExprSymbol sym = expr->list.head->expr->symbol;

  Text macroName;
  macroName.length = sym.length;
  macroName.value = sym.value;

  Expr input;
  input.type = N_LIST;
  listInitContents(&input.list);
  input.list.length = expr->list.length - 1;
  input.list.head = expr->list.head->next;
  input.list.tail = expr->list.tail;

  Expr output;

  throws(tryExpand(ctx->options.expander, macroName, &input, &output, error));
  throws(tryFormAnalyzeContents(ctx, &output, form, error));

  return R_SUCCESS;

  failure:
  return ret;
}

RetVal tryFormAnalyzeContents(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error) {

  // copy expression source metadata
  form->source = expr->source;

  RetVal ret;

  switch (expr->type) {

    // constants
    case N_STRING:
    case N_NUMBER:
    case N_KEYWORD:
    case N_BOOLEAN:
    case N_NIL: {
      form->type = F_CONST;
      throws(tryConstantAnalyze(expr, &form->constant, error));
      break;
    }

    case N_SYMBOL: {
      throws(trySymbolAnalyze(ctx, expr, form, error));
      break;
    }

    case N_LIST: {

      // empty list
      if (expr->list.length == 0) {
        form->type = F_CONST;
        throws(tryConstantAnalyze(expr, &form->constant, error));
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

        if (wcscmp(sym, L"let") == 0) {
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
          throws(tryQuoteAnalyze(expr, &form->constant, error));
          break;
        }

        if (wcscmp(sym, L"builtin") == 0) {
          form->type = F_BUILTIN;
          throws(tryBuiltinAnalyze(ctx, expr, &form->builtin, error));
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

  tryMalloc(form, sizeof(Form), "Form");
  throws(tryFormAnalyzeContents(ctx, expr, form, error));

  *ptr = form;
  return R_SUCCESS;

  failure:
    if (form != NULL) {
      free(form);
    }
    return ret;
}

void formFreeContents(Form* form) {
  if (form != NULL) {
    switch (form->type) {
      case F_CONST:
        constantFreeContents(form->constant);
        break;
      case F_IF:
        ifFreeContents(&form->iff);
        break;
      case F_LET:
        letFreeContents(&form->let);
        break;
      case F_DEF:
        defFreeContents(&form->def);
        break;
      case F_ENV_REF:
        envRefFreeContents(&form->envRef);
        break;
      case F_VAR_REF:
        varRefFreeContents(&form->varRef);
        break;
      case F_FN:
        fnFreeContents(&form->fn);
        break;
      case F_BUILTIN:
        builtinFreeContents(&form->builtin);
        break;
      case F_FN_CALL:
        fnCallFreeContents(&form->fnCall);
        break;
      case F_NONE:
        break;
    }
  }
}

void formFree(Form* form) {
  if (form != NULL) {
    formFreeContents(form);
    free(form);
  }
}

void rootInitContents(FormRoot *root) {
  bindingTableInitContents(&root->table);
  root->form = NULL;
}

void rootFreeContents(FormRoot *root) {
  if (root != NULL) {
    bindingTableFreeContents(&root->table);
    if (root->form != NULL) {
      formFree(root->form);
      root->form = NULL;
    }
  }
}

void rootFree(FormRoot *root) {
  if (root != NULL) {
    rootFreeContents(root);
    free(root);
  }
}

void analyzerContextInitContents(AnalyzerContext *ctx) {
  ctx->fnCount = 0;
  bindingTablesInitContents(&ctx->bindingTables);
  resolverStackInitContents(&ctx->resolverStack);
  analyzeOptionsInitContents(&ctx->options);
}

void analyzerContextFreeContents(AnalyzerContext *ctx) {
  if (ctx != NULL) {
    ctx->fnCount = 0;
    bindingTablesFreeContents(&ctx->bindingTables);
    resolverStackFreeContents(&ctx->resolverStack);
  }
}

void analyzeOptionsInitContents(AnalyzeOptions *options) {
  options->expander = NULL;
}

RetVal tryFormAnalyzeOptions(AnalyzeOptions options, Expr* expr, FormRoot **ptr, Error *error) {
  RetVal ret;
  AnalyzerContext ctx;

  analyzerContextInitContents(&ctx);
  ctx.options = options;

  FormRoot *root = NULL;
  tryMalloc(root, sizeof(FormRoot), "FormRoot");
  rootInitContents(root);
  throws(tryPushBindingTable(&ctx.bindingTables, &root->table, error));

  throws(_tryFormAnalyze(&ctx, expr, &root->form, error));

  throws(tryPopBindingTable(&ctx.bindingTables, error));
  analyzerContextFreeContents(&ctx);

  *ptr = root;
  return R_SUCCESS;

  failure:
    rootFree(root);
    analyzerContextFreeContents(&ctx);
    return ret;
}

RetVal tryFormAnalyze(Expr* expr, FormRoot **ptr, Error *error) {
  AnalyzeOptions options;
  analyzeOptionsInitContents(&options);
  return tryFormAnalyzeOptions(options, expr, ptr, error);
}

// remove the duplication for init found in tryFormDeepCopy
// do we really need the deep copy behavior now?
// this is definitely out of date now, fields have changed at least for the lexical bindings

//RetVal tryFormDeepCopy(Form *from, Form **ptr, Error *error) {
//  RetVal ret;
//
//  Form *to;
//  tryMalloc(to, sizeof(Form), "Form");
//
//  switch (from->type) {
//    case F_CONST:
//      throws(tryExprDeepCopy(from->constant, &to->constant, error));
//      break;
//
//    case F_IF:
//      throws(tryFormDeepCopy(from->iff.test, &to->iff.test, error));
//      throws(tryFormDeepCopy(from->iff.ifBranch, &to->iff.ifBranch, error));
//      if (from->iff.elseBranch != NULL) {
//        throws(tryFormDeepCopy(from->iff.elseBranch, &to->iff.elseBranch, error));
//      }
//      break;
//
//    case F_LET:
//      to->let.numBindings = from->let.numBindings;
//      tryMalloc(to->let.bindings, sizeof(LetBinding) * to->let.numBindings, "LetBinding array");
//
//      for (int i=0; i<to->let.numBindings; i++) {
//        LexicalBinding *fromBinding = &from->let.bindings[i];
//        LexicalBinding *toBinding = &to->let.bindings[i];
//
//        toBinding->nameLength = fromBinding->nameLength;
//        toBinding->source = fromBinding->source;
//
//        throws(tryCopyText(fromBinding->name, &toBinding->name, toBinding->nameLength, error));
//        throws(tryFormDeepCopy(fromBinding->value, &toBinding->value, error));
//      }
//
//      to->let.numForms = from->let.numForms;
//      tryMalloc(to->let.bindings, sizeof(Form) * to->let.numForms, "Form array");
//
//      for (int i=0; i<to->let.numForms; i++) {
//        Form *fromForm = &from->let.forms[i];
//        Form *toForm = &to->let.forms[i];
//        throws(tryFormDeepCopy(fromForm, &toForm, error));
//      }
//
//      break;
//
//    case F_DEF:
//      to->def.nameLength = from->def.nameLength;
//      throws(tryCopyText(from->def.name, &to->def.name, to->def.nameLength, error));
//      throws(tryFormDeepCopy(from->def.value, &to->def.value, error));
//      break;
//
//    case F_ENV_REF:
//      to->envRef.type = from->envRef.type;
//      to->envRef.typeIndex = from->envRef.typeIndex;
//      break;
//
//    case F_VAR_REF:
//      // TODO
//      break;
//
//    case F_FN:
//      to->fn.numArgs = from->fn.numArgs;
//      tryMalloc(to->fn.args, sizeof(FormFnArg) * to->fn.numArgs, "FormFnArg array");
//
//      for (int i=0; i<to->fn.numArgs; i++) {
//        FormFnArg *fromArg = &from->fn.args[i];
//        FormFnArg *toArg = &to->fn.args[i];
//
//        toArg->nameLength = fromArg->nameLength;
//        toArg->source = fromArg->source;
//
//        throws(tryCopyText(fromArg->name, &toArg->name, toArg->nameLength, error));
//      }
//
//      to->fn.numForms = from->fn.numForms;
//      tryMalloc(to->fn.forms, sizeof(Form) * to->fn.numForms, "Form array");
//
//      for (int i=0; i<to->fn.numForms; i++) {
//        Form *fromForm = &from->fn.forms[i];
//        Form *toForm = &to->fn.forms[i];
//        throws(tryFormDeepCopy(fromForm, &toForm, error));
//      }
//
//      break;
//
//    case F_BUILTIN:
//      to->builtin.nameLength = from->builtin.nameLength;
//      throws(tryCopyText(from->builtin.name, &to->builtin.name, to->builtin.nameLength, error));
//
//      to->builtin.numArgs = from->builtin.numArgs;
//      tryMalloc(to->builtin.args, sizeof(Form) * to->builtin.numArgs, "Form array");
//
//      for (int i=0; i<to->builtin.numArgs; i++) {
//        Form *fromArg = &from->builtin.args[i];
//        Form *toArg = &to->builtin.args[i];
//        throws(tryFormDeepCopy(fromArg, &toArg, error));
//      }
//
//      break;
//
//    case F_FN_CALL:
//
//      throws(tryFormDeepCopy(from->fnCall.fnCallable, &to->fnCall.fnCallable, error));
//
//      to->fnCall.numArgs = from->fnCall.numArgs;
//      tryMalloc(to->fnCall.args, sizeof(Form) * to->fnCall.numArgs, "Form array");
//
//      for (int i=0; i<to->fnCall.numArgs; i++) {
//        Form *fromForm = &from->fnCall.args[i];
//        Form *toForm = &to->fnCall.args[i];
//        throws(tryFormDeepCopy(fromForm, &toForm, error));
//      }
//
//      break;
//
//    case F_NONE:
//      break;
//  }
//
//  to->type = from->type;
//  to->source = from->source;
//
//  *ptr = to;
//  return R_SUCCESS;
//
//  failure:
//    if (to != NULL) {
//      formFree(to);
//    }
//    return ret;
//}


// TODO: if we aren't doing variable capture yet, then it seems like we should start with a fresh binding stack here

// TODO: there's nothing wrong with both remembering which bindings are arguments, and which are env bindings, but
// TODO: but also tracking a uniform typeIndex for both from the perspective of them both being 'locals'. this would
// allow us to stop being aware of this in the emitter. *this uniformity would include a single 'addBinding' function
// that takes a struct which is always the same, regardless of the type of binding being added

// TODO: if we were to do variable capture, the resolution would probably take into account both the 'new' stack
// TODO: as well as falling back to the 'previous' one
// this suggests that the binding stack is a parameter value, not a singleton
