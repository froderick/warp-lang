#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "analyzer.h"
#include "utils.h"

/*
 * Core analyzer behavior.
 */

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

uint16_t countBindings(EnvBindingStack *stack) {
  EnvBindingScope *scope = stack->head;
  uint16_t numBindings = 0;
  while (scope != NULL) {
    numBindings += scope->numBindings;
    scope = scope->next;
  }
  return numBindings;
}

RetVal addEnvBinding(EnvBindingStack *stack, EnvBinding e, Error *error) {
  RetVal ret;

  EnvBindingScope *scope = stack->head;

  if (scope->numBindings + 1 > scope->allocNumBindings) {
    throwInternalError(error, "attempted to add more bindings than were allocated");
  }

  scope->bindings[scope->numBindings] = e;
  scope->numBindings = scope->numBindings + 1;
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
  e.index = countBindings(stack);

  throws(addEnvBinding(stack, e, error));

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
  e.index = countBindings(stack);

  throws(addEnvBinding(stack, e, error));

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

uint64_t getExprPosition(Expr *expr) {
  return expr->source.position;
}

uint64_t getFormPosition(Form *form) {
  return form->source.position;
}

/*
 * Analyzers for the different types of forms.
 */

RetVal tryFormAnalyzeContents(EnvBindingStack *bindingStack, Expr* expr, Form *form, Error *error);

void ifFreeContents(FormIf *iff);

RetVal tryIfAnalyze(EnvBindingStack *bindingStack, Expr* ifExpr, FormIf *iff, Error *error) {

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

  throws(tryFormAnalyze(bindingStack, testExpr, &iff->test, error));
  throws(tryFormAnalyze(bindingStack, ifBranchExpr, &iff->ifBranch, error));
  throws(tryFormAnalyze(bindingStack, elseBranchExpr, &iff->elseBranch, error));

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

RetVal tryLetAnalyze(EnvBindingStack *bindingStack, Expr* letExpr, FormLet *let, Error *error) {

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
  throws(tryPushScope(bindingStack, let->numBindings, error));
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
    throws(addLexicalBinding(bindingStack, b, error));
    throws(tryFormAnalyze(bindingStack, bindingElem->next->expr, &b->value, error));

    bindingElem = bindingElem->next->next;
  }

  // create the forms within this lexical scope
  let->numForms = letExpr->list.length - 2;
  tryMalloc(let->forms, sizeof(Form) * let->numForms, "Forms array");

  ListElement *exprElem = letExpr->list.head->next->next;
  for (int i=0; i<let->numForms; i++) {
    Form *thisForm = let->forms + i;
    throws(tryFormAnalyzeContents(bindingStack, exprElem->expr, thisForm, error));
    exprElem = exprElem->next;
  }

  // discard the registered bindings from the environment stack
  popScope(bindingStack);

  return R_SUCCESS;

  failure:
    letFreeContents(let);
    if (scopePushed) {
      popScope(bindingStack);
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

RetVal tryDefAnalyze(EnvBindingStack *bindingStack, Expr* defExpr, FormDef *def, Error *error) {

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
    throws(tryFormAnalyze(bindingStack, defExpr->list.head->next->next->expr, &def->value, error));
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

RetVal tryFnAnalyze(EnvBindingStack *parentBindingStack, Expr* fnExpr, FormFn *fn, Error *error) {

  RetVal ret;

  // things that get cleaned up always
  EnvBindingStack fnBindingStack;

  // things that get cleaned up on failure
  fn->nameLength = 0;
  fn->name = NULL;
  fn->numForms = 0;
  fn->args = NULL;
  fn->numForms = 0;
  fn->forms = NULL;

  // sanity checking
  uint64_t pos = getExprPosition(fnExpr);
  ListElement *itr = fnExpr->list.head->next;
  uint16_t numForms = fnExpr->list.length - 1;

  if (itr == NULL) {
    throwSyntaxError(error, pos, "the 'fn' special form requires at least one parameter");
  }

  // the optional function name
  bool hasName = false;
  if (itr->expr->type == N_SYMBOL) {

    fn->nameLength = itr->expr->symbol.length;
    throws(tryCopyText(itr->expr->symbol.value, &fn->name, fn->nameLength, error));

    itr = itr->next;
    numForms = numForms - 1;

    hasName = true;
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
  numForms = numForms - 1;

  // create new binding stack, initialized with the fn args as the first bindings
  envBindingStackInit(&fnBindingStack);
  throws(tryPushScope(&fnBindingStack, fn->numArgs, error));
  for (uint64_t i=0; i<fn->numArgs; i++) {
    throws(addArgBinding(&fnBindingStack, &fn->args[i], error));
  }

  // create the forms within this fn lexical scope
  fn->numForms = numForms;
  tryMalloc(fn->forms, sizeof(Form) * fn->numForms, "Forms array");

  for (int i=0; i<fn->numForms; i++) {
    Expr *expr = itr->expr;
    Form *thisForm = fn->forms + i;
    throws(tryFormAnalyzeContents(&fnBindingStack, expr, thisForm, error));
    itr = itr->next;
  }

  envBindingStackFreeContents(&fnBindingStack);

  return R_SUCCESS;

  failure:
    fnFreeContents(fn);
    envBindingStackFreeContents(&fnBindingStack);
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

RetVal tryBuiltinAnalyze(EnvBindingStack *bindingStack, Expr *expr, FormBuiltin *builtin, Error *error) {

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
    throws(tryFormAnalyzeContents(bindingStack, argExpr->expr, arg, error));
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

RetVal tryEnvRefAnalyze(EnvBindingStack *bindingStack, Expr *expr, EnvBinding *binding, FormEnvRef *envRef, Error *error) {

  envRef->type = binding->type;
  envRef->index = binding->index;

  return R_SUCCESS;
}

void envRefFreeContents(FormEnvRef *ref) {
  // nothing to do
}

RetVal tryVarRefAnalyze(EnvBindingStack *bindingStack, Expr *expr, FormVarRef *varRef, Error *error) {
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

RetVal tryFnCallAnalyze(EnvBindingStack *bindingStack, Expr *expr, FormFnCall *fnCall, Error *error) {

  RetVal ret;

  fnCall->fnCallable = NULL;
  fnCall->args = NULL;

  throws(tryFormAnalyze(bindingStack, expr->list.head->expr, &fnCall->fnCallable, error));
  throws(assertFnCallable(fnCall->fnCallable, error));

  fnCall->numArgs = expr->list.length - 1;
  tryMalloc(fnCall->args, sizeof(Form) * fnCall->numArgs, "Form array");

  ListElement *argExpr = expr->list.head->next;
  for (int i=0; i<fnCall->numArgs; i++) {
    Form *arg = fnCall->args + i;
    throws(tryFormAnalyzeContents(bindingStack, argExpr->expr, arg, error));
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

RetVal tryFormAnalyzeContents(EnvBindingStack *bindingStack, Expr* expr, Form *form, Error *error) {

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

      if ((envBinding = findBinding(bindingStack, sym)) != NULL) {
        form->type = F_ENV_REF;
        throws(tryEnvRefAnalyze(bindingStack , expr, envBinding, &form->envRef, error));
      }
      else { // if ((var = resolveVar(analyzer, sym, expr->symbol.length)) != NULL) {
        form->type = F_VAR_REF;
        throws(tryVarRefAnalyze(bindingStack, expr, &form->varRef, error));
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
          throws(tryIfAnalyze(bindingStack, expr, &form->iff, error));
          break;
        }

        if (wcscmp(sym, L"let") == 0) {
          form->type = F_LET;
          throws(tryLetAnalyze(bindingStack, expr, &form->let, error));
          break;
        }

        if (wcscmp(sym, L"def") == 0) {
          form->type = F_DEF;
          throws(tryDefAnalyze(bindingStack, expr, &form->def, error));
          break;
        }

        if (wcscmp(sym, L"fn") == 0) {
          form->type = F_FN;
          throws(tryFnAnalyze(bindingStack, expr, &form->fn, error));
          break;
        }

        if (wcscmp(sym, L"quote") == 0) {
          form->type = F_CONST;
          throws(tryQuoteAnalyze(expr, &form->constant, error));
          break;
        }

        if (wcscmp(sym, L"builtin") == 0) {
          form->type = F_BUILTIN;
          throws(tryBuiltinAnalyze(bindingStack, expr, &form->builtin, error));
          break;
        }
      }

      // assume fn-callable
      form->type = F_FN_CALL;
      throws(tryFnCallAnalyze(bindingStack, expr, &form->fnCall, error));
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

RetVal tryFormAnalyze(EnvBindingStack *bindingStack, Expr* expr, Form **ptr, Error *error) {

  RetVal ret;
  Form *form;

  tryMalloc(form, sizeof(Form), "Form");
  throws(tryFormAnalyzeContents(bindingStack, expr, form, error));

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


