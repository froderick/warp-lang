#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "analyzer.h"
#include "utils.h"

/*
 * Core analyzer behavior.
 */

typedef struct EnvBinding {
  wchar_t *name;
  uint64_t nameLength;
  FormEnvRefType type;
  uint64_t index;
} EnvBinding;

typedef struct EnvBindingScope {
  uint64_t numBindings;
  uint64_t allocNumBindings;
  EnvBinding *bindings;
  struct EnvBindingScope *next;
  // these are denormalized counts used for computing indexes
  uint64_t numLocalBindings;
} EnvBindingScope;

typedef struct EnvBindingStack {
  uint64_t depth;
  EnvBindingScope *head;
} EnvBindingStack;

typedef struct AnalyzerContext {
  EnvBindingStack bindingStack;
  uint16_t fnCount;
} AnalyzerContext;

RetVal _tryFormAnalyze(AnalyzerContext *ctx, Expr* expr, Form **ptr, Error *error);

void envBindingStackInit(EnvBindingStack *bindingStack);
void envBindingStackFreeContents(EnvBindingStack *bindingStack);

void analyzerContextInitContents(AnalyzerContext *ctx);
void analyzerContextFreeContents(AnalyzerContext *ctx);

RetVal tryFormDeepCopy(Form *from, Form **to, Error *error);

void scopeFree(EnvBindingScope *scope);

RetVal tryScopeMake(uint64_t allocNumBindings, EnvBindingScope **ptr, Error *error) {

  RetVal ret;

  EnvBindingScope *scope;
  tryMalloc(scope, sizeof(EnvBindingScope), "EnvBindingScope");

  scope->allocNumBindings = allocNumBindings;
  scope->numBindings = 0;
  scope->bindings = NULL;
  scope->next = NULL;
  scope->numLocalBindings = 0;

  tryMalloc(scope->bindings, sizeof(EnvBinding) * scope->allocNumBindings, "EnvBinding array");

  for (int i=0; i<scope->numBindings; i++) {
    EnvBinding *b = scope->bindings + i;
    b->nameLength = 0;
    b->name = NULL;
    b->type = RT_NONE;
    b->index = 0;
  }

  *ptr = scope;
  return R_SUCCESS;

  failure:
  scopeFree(scope);
  return ret;
}

void bindingFreeContents(EnvBinding *binding) {
  if (binding != NULL) {
    if (binding->name != NULL) {
      free(binding->name);
      binding->name = NULL;
    }
  }
}

void scopeFree(EnvBindingScope *scope) {
  if (scope != NULL) {
    if (scope->bindings != NULL) {
      for (int i=0; i<scope->numBindings; i++) {
        bindingFreeContents(scope->bindings + i);
      }
      free(scope->bindings);
    }
    free(scope);
  }
}

RetVal tryPushScope(EnvBindingStack *stack, uint16_t allocNumBindings, Error *error) {
  RetVal ret;

  EnvBindingScope *scope;
  throws(tryScopeMake(allocNumBindings, &scope, error));

  scope->next = stack->head;
  stack->head = scope;
  stack->depth = stack->depth + 1;

  return R_SUCCESS;

  failure:
    return ret;
}

void popScope(EnvBindingStack *stack) {

  EnvBindingScope *scope = stack->head;
  stack->head = scope->next;
  stack->depth = stack->depth - 1;

  scopeFree(scope);
}

uint16_t countLocalBindings(EnvBindingStack *stack) {
  EnvBindingScope *scope = stack->head;
  uint16_t numBindings = 0;
  while (scope != NULL) {
    numBindings += scope->numLocalBindings;
    scope = scope->next;
  }
  return numBindings;
}

typedef enum BindingClass {
  BC_LOCAL,
  BC_FN
} BindingClass;

RetVal addEnvBinding(EnvBindingStack *stack, EnvBinding e, BindingClass class, Error *error) {
  RetVal ret;

  EnvBindingScope *scope = stack->head;

  if (scope->numBindings + 1 > scope->allocNumBindings) {
    throwInternalError(error, "attempted to add more bindings than were allocated");
  }

  scope->bindings[scope->numBindings] = e;
  scope->numBindings = scope->numBindings + 1;

  if (class == BC_LOCAL) {
    scope->numLocalBindings = scope->numLocalBindings + 1;
  }
  else if (class == BC_FN) {
    // do nothing
  }
  else {
    throwInternalError(error, "unsupported binding class: %u", class);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal addLexicalBinding(EnvBindingStack *stack, LexicalBinding *b, Error *error) {
  RetVal ret;

  EnvBinding e;
  e.nameLength = b->nameLength;
  throws(tryCopyText(b->name, &e.name, e.nameLength, error));
  e.type = RT_LOCAL;
  e.index = countLocalBindings(stack);

  throws(addEnvBinding(stack, e, BC_LOCAL, error));

  // dirty but convenient, technicaly this function 'belongs' to the let impl
  b->index = e.index;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal addArgBinding(EnvBindingStack *stack, FormFnArg *arg, Error *error) {
  RetVal ret;

  EnvBinding e;
  e.nameLength = arg->nameLength;
  throws(tryCopyText(arg->name, &e.name, e.nameLength, error));
  e.type = RT_ARG;
  e.index = countLocalBindings(stack);

  throws(addEnvBinding(stack, e, BC_LOCAL, error));

  return R_SUCCESS;

  failure:
    return ret;
}

// is there a serious bug here? if we base the index of a local or fn-ref on the stack depth
// and the stack increases and decreases, and increases again, surely this will break?
//
// no, I thought about it. this makes very good use of the locals, reusing their locations
// when they go out of scope. it does mean that the meaning of a local may change over time,
// but that's been ok so far.

RetVal addFnBinding(EnvBindingStack *stack, FormFn *fn, Error *error) {
  RetVal ret;

  EnvBinding e;
  e.nameLength = fn->nameLength;
  throws(tryCopyText(fn->name, &e.name, e.nameLength, error));
  e.type = RT_FN;
  e.index = fn->id;

  throws(addEnvBinding(stack, e, BC_FN, error));

  return R_SUCCESS;

  failure:
  return ret;
}

EnvBinding* findBinding(EnvBindingStack *stack, wchar_t *bindingName) {
  EnvBindingScope *scope = stack->head;
  while (scope != NULL) {
    for (int i=0; i<scope->numBindings; i++) {
      if (wcscmp(bindingName, scope->bindings[i].name) == 0) {
        return &scope->bindings[i];
      }
    }
    scope = scope->next; // TODO: not sure this will work for nested bindings
  }
  return NULL;
}

void envBindingStackInit(EnvBindingStack *bindingStack) {
  bindingStack->head = NULL;
  bindingStack->depth = 0;
}

void envBindingStackFreeContents(EnvBindingStack *stack) {
  if (stack != NULL) {
    while (stack->head != NULL) {
      EnvBindingScope *next = stack->head->next;
      scopeFree(stack->head);
      stack->head = next;
      stack->depth = stack->depth - 1;
    }
  }
}

void analyzerContextInitContents(AnalyzerContext *ctx) {
  envBindingStackInit(&ctx->bindingStack);
}

void analyzerContextFreeContents(AnalyzerContext *ctx) {
  if (ctx != NULL) {
    envBindingStackFreeContents(&ctx->bindingStack);
  }
}

uint64_t getExprPosition(Expr *expr) {
  return expr->source.position;
}

uint64_t getFormPosition(Form *form) {
  return form->source.position;
}

/*
 * Analyzers for the different types of forms.
 */

RetVal tryFormAnalyzeContents(AnalyzerContext *ctx, Expr* expr, Form *form, Error *error);

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

void letFreeContents(FormLet *let);

/*
 * purposes of the environment stack tracking
 * - determine the value of a binding reference, by matching its name to the nearest binding by that name in the stack
 * - the value includes the type of the reference (arg or local) as well as the index by which it can be identified
 */

RetVal tryLetAnalyze(AnalyzerContext *ctx, Expr* letExpr, FormLet *let, Error *error) {

  let->numBindings = 0;
  let->bindings = NULL;
  let->numForms = 0;
  let->forms = NULL;

  RetVal ret;

  // things that get cleaned up on failure
  bool scopePushed = false;

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
  tryMalloc(let->bindings, sizeof(LexicalBinding) * let->numBindings, "LexicalBinding array");

  // register the bindings in the environment stack
  throws(tryPushScope(&ctx->bindingStack, let->numBindings, error));
  scopePushed = true;

  // initialize the bindings
  ListElement *bindingElem = bindingsExpr->list.head;
  for (int i=0; bindingElem != NULL; i++) {

    if (bindingElem->expr->type != N_SYMBOL) {
      throwSyntaxError(error, pos, "only symbols can be bound as names");
    }

    LexicalBinding *b = let->bindings + i;
    b->nameLength = bindingElem->expr->symbol.length;
    b->source = bindingElem->expr->source;

    throws(tryCopyText(bindingElem->expr->symbol.value, &b->name, b->nameLength, error));
    throws(addLexicalBinding(&ctx->bindingStack, b, error));
    throws(_tryFormAnalyze(ctx, bindingElem->next->expr, &b->value, error));

    bindingElem = bindingElem->next->next;
  }

  // create the forms within this lexical scope
  let->numForms = letExpr->list.length - 2;
  tryMalloc(let->forms, sizeof(Form) * let->numForms, "Forms array");

  ListElement *exprElem = letExpr->list.head->next->next;
  for (int i=0; i<let->numForms; i++) {
    Form *thisForm = let->forms + i;
    throws(tryFormAnalyzeContents(ctx, exprElem->expr, thisForm, error));
    exprElem = exprElem->next;
  }

  // discard the registered bindings from the environment stack
  popScope(&ctx->bindingStack);

  return R_SUCCESS;

  failure:
    letFreeContents(let);
    if (scopePushed) {
      popScope(&ctx->bindingStack);
    }
    return ret;
}

void letFreeContents(FormLet *let) {
  if (let != NULL) {
    if (let->bindings != NULL) {
      for (int i=0; i<let->numBindings; i++) {
        LexicalBinding *b = let->bindings + i;
        if (b->name != NULL) {
          free(b->name);
        }
        if (b->value != NULL) {
          formFree(b->value);
        }
      }
      free(let->bindings);
      let->bindings = NULL;
      let->numBindings = 0;
    }
    if (let->forms != NULL) {
      for (int i=0; i<let->numForms; i++) {
        formFreeContents(let->forms + i);
      }
      free(let->forms);
      let->forms = NULL;
      let->numForms = 0;
    }
  }
}

void defFreeContents(FormDef *let);

RetVal tryDefAnalyze(AnalyzerContext *ctx, Expr* defExpr, FormDef *def, Error *error) {

  def->name = NULL;
  def->nameLength = 0;
  def->value = NULL;

  RetVal ret;

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

  def->nameLength = symbol->symbol.length;
  throws(tryCopyText(symbol->symbol.value, &def->name, def->nameLength, error));

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
    if (def->name != NULL) {
      free(def->name);
      def->name = NULL;
    }
    if (def->value != NULL) {
      formFree(def->value);
      def->value = NULL;
    };
  }
}

void fnFreeContents(FormFn *fn);

void formFnInitContents(FormFn *fn) {
  fn->hasName = false;
  fn->nameLength = 0;
  fn->name = NULL;
  fn->id = 0;
  fn->numForms = 0;
  fn->args = NULL;
  fn->numForms = 0;
  fn->forms = NULL;
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

    fn->nameLength = itr->expr->symbol.length;
    throws(tryCopyText(itr->expr->symbol.value, &fn->name, fn->nameLength, error));
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
    arg->nameLength = argElem->expr->symbol.length;
    arg->name = NULL;
    arg->source = argElem->expr->source;

    throws(tryCopyText(argElem->expr->symbol.value, &arg->name, arg->nameLength, error));

    argElem = argElem->next;
  }
  itr = itr->next;

  uint16_t nonFormElems = 2; // 'fn' and 'args list'
  if (fn->hasName) {
    nonFormElems = nonFormElems + 1; // optional function name
  }

  fn->numForms = fnExpr->list.length - nonFormElems;
  tryMalloc(*formElements, sizeof(Expr) * fn->numForms, "Expr array");

  for (int i=0; i<fn->numForms; i++) {
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
      if (last->let.numForms > 0) {
        Form *letLast = &last->let.forms[last->let.numForms - 1];
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
  if (fn->numForms > 0) {
    Form *last = &fn->forms[fn->numForms - 1];
    _markTailCalls(last);
  }
}

RetVal tryFnAnalyze(AnalyzerContext *parentContext, Expr* fnExpr, FormFn *fn, Error *error) {
  RetVal ret;

  // things that get cleaned up always
  AnalyzerContext fnContext;
  Expr *formElements = NULL;

  formFnInitContents(fn);
  throws(tryFnParse(fnExpr, fn, &formElements, error));

  // create new binding stack, initialized with the fn args as the first bindings
  analyzerContextInitContents(&fnContext);
  fnContext.fnCount = parentContext->fnCount;

  uint16_t numBindings = fn->hasName ? fn->numArgs + 1 : fn->numArgs;
  throws(tryPushScope(&fnContext.bindingStack, numBindings, error));

  if (fn->hasName) {
    fn->id = fnContext.fnCount;
    fnContext.fnCount = fnContext.fnCount + 1;
    throws(addFnBinding(&fnContext.bindingStack, fn, error));
  }

  for (uint64_t i=0; i<fn->numArgs; i++) {
    throws(addArgBinding(&fnContext.bindingStack, &fn->args[i], error));
  }

  // create the forms within this fn lexical scope
  tryMalloc(fn->forms, sizeof(Form) * fn->numForms, "Forms array");
  for (int i=0; i<fn->numForms; i++) {
    Expr expr = formElements[i];
    Form *thisForm = fn->forms + i;
    throws(tryFormAnalyzeContents(&fnContext, &expr, thisForm, error));
  }

  parentContext->fnCount = fnContext.fnCount;

  markTailCalls(fn);

  ret = R_SUCCESS;
  goto done;

  failure:
    fnFreeContents(fn);
    goto done;

  done:
    if (formElements != NULL) {
      free(formElements);
    }
    analyzerContextFreeContents(&fnContext);
    return ret;
}

// TODO: if we aren't doing variable capture yet, then it seems like we should start with a fresh binding stack here

// TODO: there's nothing wrong with both remembering which bindings are arguments, and which are env bindings, but
// TODO: but also tracking a uniform index for both from the perspective of them both being 'locals'. this would
// allow us to stop being aware of this in the emitter. *this uniformity would include a single 'addBinding' function
// that takes a struct which is always the same, regardless of the type of binding being added

// TODO: if we were to do variable capture, the resolution would probably take into account both the 'new' stack
// TODO: as well as falling back to the 'previous' one
// this suggests that the binding stack is a parameter value, not a singleton

void fnFreeContents(FormFn *fn) {
  if (fn != NULL) {
    fn->nameLength = 0;
    if (fn->name != NULL) {
      free(fn->name);
      fn->name = NULL;
    }
    if (fn->args != NULL) {
      for (int i=0; i<fn->numArgs; i++) {
        FormFnArg *arg = fn->args + i;
        if (arg->name != NULL) {
          free(arg->name);
          arg->name = NULL;
        }
        // don't need to free expr
      }
      free(fn->args);
      fn->args = NULL;
      fn->numArgs = 0;
    }
    if (fn->forms != NULL) {
      for (int i = 0; i < fn->numForms; i++) {
        formFreeContents(fn->forms + i);
      }
      free(fn->forms);
      fn->forms = NULL;
      fn->numForms = 0;
    }
  }
}

void builtinFreeContents(FormBuiltin *builtin);

RetVal tryBuiltinAnalyze(AnalyzerContext *ctx, Expr *expr, FormBuiltin *builtin, Error *error) {

  RetVal ret;

  builtin->numArgs = 0;
  builtin->args = NULL;
  builtin->name = NULL;
  builtin->nameLength = 0;

  Expr *name = expr->list.head->next->expr;
  if (name->type != N_KEYWORD) {
    throwSyntaxError(error, name->source.position, "the 'builtin' special form requires the first parameter to be a keyword");
  }

  builtin->nameLength = name->keyword.length;
  throws(tryCopyText(name->keyword.value, &builtin->name, builtin->nameLength, error));

  builtin->numArgs = expr->list.length - 2;
  tryMalloc(builtin->args, sizeof(Form) * builtin->numArgs, "Form array");

  ListElement *argExpr = expr->list.head->next->next;
  for (int i=0; i<builtin->numArgs; i++) {
    Form *arg = builtin->args + i;
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
    if (builtin->name != NULL) {
      free(builtin->name);
      builtin->name = NULL;
      builtin->nameLength = 0;
    }
    if (builtin->args != NULL) {
      for (int i=0; i<builtin->numArgs; i++) {
        formFreeContents(builtin->args+ i);
      }
      free(builtin->args);
      builtin->args = NULL;
      builtin->numArgs = 0;
    }
  }
}

typedef enum BindingSource {
  BT_CAPTURED,
  BT_LOCAL,
} BindingSource;

typedef enum BindingType {
  BT_LET,
  BT_FN_REF,
  BT_FN_ARG,
} BindingType;

typedef struct Binding {
  BindingSource source;
  BindingType type;
  uint16_t index;
} Binding;

typedef struct BindingTable {
  uint16_t numBindings;
  Binding *bindings;
} BindingTable;

typedef struct Forms {
  uint16_t numForms;
  Form *forms;
} Forms;

typedef struct FormLet2 {
  Forms forms;
} FormLet2;

typedef struct FormRoot {
  BindingTable table;
  Forms forms;
} FormRoot;

typedef struct FnRoot {
  BindingTable table;
  Forms forms;
} FnRoot;

RetVal tryEnvRefAnalyze(AnalyzerContext *ctx, Expr *expr, EnvBinding *binding, FormEnvRef *envRef, Error *error) {

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
   * let bindings are discovered. References within a function use the binding index to identify them.
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
   *     - a let binding (includes unique let index within a function)
   *     - a function reference binding (only one defined per function)
   *     - a function arg binding (includes arg index within a function)
   *     - a function definition boundary, referencing a FormFn
   *     instead of pushing/popping scopes, the analyzer can save the current stack height, and then pop back down to it
   *     efficiently when it is done with a particular binding scope
   *
   * - the analyzer needs to walk through the binding stack to resolve binding references as it encounters them. it
   *   starts at the top of the stack and keeps moving down until it finds a binding with a matching name, or hits the
   *   bottom of the stack and errors out because the binding reference cannot be resolved
   *
   * - the analyzer must emit a FormEnvRef that identifies which binding it references. bindings are identified by type
   *   and index. FormEnvRefs can only identify bindings that originate from within a function or top-level code unit.
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
   *       are referenced by index. these indexes must be computed
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

  envRef->type = binding->type;
  envRef->index = binding->index;

  return R_SUCCESS;
}

void envRefFreeContents(FormEnvRef *ref) {
  // nothing to do
}

RetVal tryVarRefAnalyze(AnalyzerContext *ctx, Expr *expr, FormVarRef *varRef, Error *error) {
  RetVal ret;

  if (expr->type != N_SYMBOL) {
    throwSyntaxError(error, getExprPosition(expr), "var refs must be symbols: '%i'", expr->type);
  }

  varRef->nameLength = expr->symbol.length;
  throws(tryCopyText(expr->symbol.value, &varRef->name, varRef->nameLength, error));

  return R_SUCCESS;

  failure:
    return ret;
}

void varRefFreeContents(FormVarRef *ref) {
  if (ref != NULL) {
    if (ref->name != NULL) {
      free(ref->name);
      ref->name = NULL;
    }
    ref->nameLength = 0;
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

void fnCallFreeContents(FormFnCall *fnCall);

RetVal tryFnCallAnalyze(AnalyzerContext *ctx, Expr *expr, FormFnCall *fnCall, Error *error) {

  RetVal ret;

  fnCall->fnCallable = NULL;
  fnCall->args = NULL;
  fnCall->numArgs = 0;
  fnCall->tailPosition = false;

  throws(_tryFormAnalyze(ctx, expr->list.head->expr, &fnCall->fnCallable, error));
  throws(assertFnCallable(fnCall->fnCallable, error));

  fnCall->numArgs = expr->list.length - 1;
  tryMalloc(fnCall->args, sizeof(Form) * fnCall->numArgs, "Form array");

  ListElement *argExpr = expr->list.head->next;
  for (int i=0; i<fnCall->numArgs; i++) {
    Form *arg = fnCall->args + i;
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
    if (fnCall->args != NULL) {
      for (int i=0; i<fnCall->numArgs; i++) {
        formFreeContents(fnCall->args + i);
      }
      free(fnCall->args);
      fnCall->args = NULL;
      fnCall->numArgs = 0;
    }
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

      wchar_t *sym = expr->symbol.value;
      EnvBinding *envBinding;

      if ((envBinding = findBinding(&ctx->bindingStack, sym)) != NULL) {
        form->type = F_ENV_REF;
        throws(tryEnvRefAnalyze(ctx, expr, envBinding, &form->envRef, error));
      }
      else { // if ((var = resolveVar(analyzer, sym, expr->symbol.length)) != NULL) {
        form->type = F_VAR_REF;
        throws(tryVarRefAnalyze(ctx, expr, &form->varRef, error));
      }
//      else {
//        throwSyntaxError(error, getExprPosition(expr), "cannot resolve symbol: '%ls'", sym);
//      }
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

RetVal tryFormAnalyze(Expr* expr, Form **form, Error *error) {

  RetVal ret;
  AnalyzerContext ctx;

  analyzerContextInitContents(&ctx);

  throws(_tryFormAnalyze(&ctx, expr, form, error));

  analyzerContextFreeContents(&ctx);
  return R_SUCCESS;

  failure:
  if (form != NULL) {
    free(form);
  }
  analyzerContextFreeContents(&ctx);
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

// TODO: remove the duplication for init found in tryFormDeepCopy
// TODO: do we really need the deep copy behavior now?
// TODO: this is definitely out of date now, fields have changed at least for the lexical bindings

RetVal tryFormDeepCopy(Form *from, Form **ptr, Error *error) {
  RetVal ret;

  Form *to;
  tryMalloc(to, sizeof(Form), "Form");

  switch (from->type) {
    case F_CONST:
      throws(tryExprDeepCopy(from->constant, &to->constant, error));
      break;

    case F_IF:
      throws(tryFormDeepCopy(from->iff.test, &to->iff.test, error));
      throws(tryFormDeepCopy(from->iff.ifBranch, &to->iff.ifBranch, error));
      if (from->iff.elseBranch != NULL) {
        throws(tryFormDeepCopy(from->iff.elseBranch, &to->iff.elseBranch, error));
      }
      break;

    case F_LET:
      to->let.numBindings = from->let.numBindings;
      tryMalloc(to->let.bindings, sizeof(LexicalBinding) * to->let.numBindings, "LexicalBinding array");

      for (int i=0; i<to->let.numBindings; i++) {
        LexicalBinding *fromBinding = &from->let.bindings[i];
        LexicalBinding *toBinding = &to->let.bindings[i];

        toBinding->nameLength = fromBinding->nameLength;
        toBinding->source = fromBinding->source;

        throws(tryCopyText(fromBinding->name, &toBinding->name, toBinding->nameLength, error));
        throws(tryFormDeepCopy(fromBinding->value, &toBinding->value, error));
      }

      to->let.numForms = from->let.numForms;
      tryMalloc(to->let.bindings, sizeof(Form) * to->let.numForms, "Form array");

      for (int i=0; i<to->let.numForms; i++) {
        Form *fromForm = &from->let.forms[i];
        Form *toForm = &to->let.forms[i];
        throws(tryFormDeepCopy(fromForm, &toForm, error));
      }

      break;

    case F_DEF:
      to->def.nameLength = from->def.nameLength;
      throws(tryCopyText(from->def.name, &to->def.name, to->def.nameLength, error));
      throws(tryFormDeepCopy(from->def.value, &to->def.value, error));
      break;

    case F_ENV_REF:
      to->envRef.type = from->envRef.type;
      to->envRef.index = from->envRef.index;
      break;

    case F_VAR_REF:
      // TODO
      break;

    case F_FN:
      to->fn.numArgs = from->fn.numArgs;
      tryMalloc(to->fn.args, sizeof(FormFnArg) * to->fn.numArgs, "FormFnArg array");

      for (int i=0; i<to->fn.numArgs; i++) {
        FormFnArg *fromArg = &from->fn.args[i];
        FormFnArg *toArg = &to->fn.args[i];

        toArg->nameLength = fromArg->nameLength;
        toArg->source = fromArg->source;

        throws(tryCopyText(fromArg->name, &toArg->name, toArg->nameLength, error));
      }

      to->fn.numForms = from->fn.numForms;
      tryMalloc(to->fn.forms, sizeof(Form) * to->fn.numForms, "Form array");

      for (int i=0; i<to->fn.numForms; i++) {
        Form *fromForm = &from->fn.forms[i];
        Form *toForm = &to->fn.forms[i];
        throws(tryFormDeepCopy(fromForm, &toForm, error));
      }

      break;

    case F_BUILTIN:
      to->builtin.nameLength = from->builtin.nameLength;
      throws(tryCopyText(from->builtin.name, &to->builtin.name, to->builtin.nameLength, error));

      to->builtin.numArgs = from->builtin.numArgs;
      tryMalloc(to->builtin.args, sizeof(Form) * to->builtin.numArgs, "Form array");

      for (int i=0; i<to->builtin.numArgs; i++) {
        Form *fromArg = &from->builtin.args[i];
        Form *toArg = &to->builtin.args[i];
        throws(tryFormDeepCopy(fromArg, &toArg, error));
      }

      break;

    case F_FN_CALL:

      throws(tryFormDeepCopy(from->fnCall.fnCallable, &to->fnCall.fnCallable, error));

      to->fnCall.numArgs = from->fnCall.numArgs;
      tryMalloc(to->fnCall.args, sizeof(Form) * to->fnCall.numArgs, "Form array");

      for (int i=0; i<to->fnCall.numArgs; i++) {
        Form *fromForm = &from->fnCall.args[i];
        Form *toForm = &to->fnCall.args[i];
        throws(tryFormDeepCopy(fromForm, &toForm, error));
      }

      break;

    case F_NONE:
      break;
  }

  to->type = from->type;
  to->source = from->source;

  *ptr = to;
  return R_SUCCESS;

  failure:
    if (to != NULL) {
      formFree(to);
    }
    return ret;
}


