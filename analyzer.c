  #include <stdint.h>
#include <stdlib.h>

#include "analyzer.h"
#include "utils.h"

RetVal tryFormAnalyzeContents(FormAnalyzer *analyzer, Expr* expr, Form *form, Error *error);

RetVal tryNamespaceMake(wchar_t *name, uint64_t length, Namespace **ptr , Error *error) {

  RetVal ret;

  Namespace *ns;
  tryMalloc(ns, sizeof(Namespace), "Namespace");

  ns->name = NULL;
  ns->importedVars = NULL;
  ns->numImportedVars = 0;
  ns->localVars = NULL;
  ns->numLocalVars = 0;

  throws(tryCopyText(name, &ns->name, length, error));

  *ptr = ns;
  return R_SUCCESS;

  failure:
    if (ns != NULL) {
      free(ns);
    }
    return ret;
}

void namespaceFree(Namespace *ns) {
  if (ns != NULL) {
    free(ns->name);
    free(ns->importedVars);
    free(ns->localVars);
    free(ns);
  }
}

RetVal tryAnalyzerMake(FormAnalyzer **ptr, Error *error) {

  RetVal ret;

  FormAnalyzer *analyzer;
  tryMalloc(analyzer, sizeof(FormAnalyzer), "FormAnalyzer");

  Namespace *userNs;
  throws(tryNamespaceMake(L"user", wcslen(L"user"), &userNs, error));

  analyzer->currentNamespace = userNs;
  analyzer->namespaces = userNs;
  analyzer->numNamespaces = 1;
  analyzer->bindingStack.head = NULL;
  analyzer->bindingStack.depth = 0;

  *ptr = analyzer;
  return R_SUCCESS;

  failure:
    if (analyzer != NULL) {
      free(analyzer);
    }
    return ret;
}

void analyzerFree(FormAnalyzer *analyzer) {
  if (analyzer != NULL) {
    analyzer->currentNamespace = NULL;
    if (analyzer->namespaces != NULL) {
      for (int i=0; i<analyzer->numNamespaces; i++) {
        namespaceFree(&analyzer->namespaces[i]);
      }
      free(analyzer->namespaces);
    }
    free(analyzer);
  }
}

void ifFreeContents(FormIf *iff);

RetVal tryIfAnalyze(FormAnalyzer *analyzer, Expr* ifExpr, FormIf *iff, Error *error) {

  iff->expr = ifExpr;
  iff->test = NULL;
  iff->ifBranch = NULL;
  iff->elseBranch = NULL;

  RetVal ret;

  uint64_t pos = ifExpr->list.oParen->position;
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

  throws(tryFormAnalyze(analyzer, testExpr, &iff->test, error));
  throws(tryFormAnalyze(analyzer, ifBranchExpr, &iff->ifBranch, error));
  throws(tryFormAnalyze(analyzer, elseBranchExpr, &iff->elseBranch, error));

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

// (let (sym expr
//       sym expr)
//   expr1
//   expr2)

void letFreeContents(FormLet *let);

RetVal tryLetAnalyze(FormAnalyzer *analyzer, Expr* letExpr, FormLet *let, Error *error) {

  let->expr = letExpr;
  let->numBindings = 0;
  let->bindings = NULL;
  let->numForms = 0;
  let->forms = NULL;

  RetVal ret;

  // sanity checking
  uint64_t pos = letExpr->list.oParen->position;
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

  let->numBindings = bindingsExpr->list.length / 2;

  // things that get cleaned up on failure
  Form *bindingValue;

  tryMalloc(let->bindings, sizeof(LexicalBinding) * let->numBindings, "LexicalBinding array");

  ListElement *bindingElem = bindingsExpr->list.head;
  for (int i=0; i<letExpr->list.length; i += 2) {

    if (bindingElem->expr->type != N_SYMBOL) {
      throwSyntaxError(error, pos, "only symbols can be bound as names");
    }

    throws(tryFormAnalyze(analyzer, bindingElem->next->expr, &bindingValue, error));

    let->bindings[i].symbol = bindingElem->expr;
    let->bindings[i].value = bindingValue;
    bindingValue = NULL; // now part of bindings

    bindingElem = bindingElem->next->next;
  }

  let->numForms = letExpr->list.length - 2;

  tryMalloc(let->forms, sizeof(Form) * let->numForms, "Forms array");

  Expr *expr = letExpr->list.head->next->next->expr;
  for (int i=0; i<let->numForms; i++) {
    Form *thisForm = let->forms + i;
    throws(tryFormAnalyzeContents(analyzer, expr, thisForm, error));
  }

  return R_SUCCESS;

  failure:
    if (bindingValue != NULL) {
      formFree(bindingValue);
    }
    letFreeContents(let);
    return ret;
}

void letFreeContents(FormLet *let) {
  if (let != NULL) {
    if (let->bindings != NULL) {
      for (int i=0; i<let->numBindings; i++) {
        LexicalBinding *b = let->bindings + i;
        // don't need to free symbol
        formFree(b->value);
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

void fnFreeContents(FormFn *fn);

RetVal tryFnAnalyze(FormAnalyzer *analyzer, Expr* fnExpr, FormFn *fn, Error *error) {

  RetVal ret;

  fn->expr = fnExpr;
  fn->numForms = 0;
  fn->args = NULL;
  fn->numForms = 0;
  fn->forms = NULL;

  // sanity checking
  uint64_t pos = fnExpr->list.oParen->position;
  if (fnExpr->list.length < 2) {
    throwSyntaxError(error, pos, "the 'fn' special form requires at least one parameter");
  }
  Expr *argsExpr = fnExpr->list.head->next->expr;
  if (argsExpr->type != N_LIST) {
    throwSyntaxError(error, pos, "the 'fn' special form requires the first parameter to be a list");
  }

  fn->numArgs = fnExpr->list.length;
  tryMalloc(fn->args, sizeof(FormFnArg) * fn->numArgs, "FormFnArg array");

  ListElement *argElem = argsExpr->list.head;
  for (int i=0; i<fn->numArgs; i++) {

    if (argElem->expr->type != N_SYMBOL) {
      throwSyntaxError(error, pos, "only symbols can be used as function arguments");
    }

    FormFnArg *arg = fn->args + i;
    arg->expr = argElem->expr;
    arg->nameLength = arg->expr->symbol.length;
    arg->name = NULL;

    throws(tryCopyText(arg->expr->symbol.value, &arg->name, arg->nameLength, error));

    argElem = argElem->next;
  }

  fn->numForms = fnExpr->list.length - 2;
  tryMalloc(fn->forms, sizeof(Form) * fn->numForms, "Forms array");

  Expr *expr = fnExpr->list.head->next->next->expr;
  for (int i=0; i<fn->numForms; i++) {
    Form *thisForm = fn->forms + i;
    throws(tryFormAnalyzeContents(analyzer, expr, thisForm, error));
  }

  return R_SUCCESS;

  failure:
    fnFreeContents(fn);
    return ret;
}

void fnFreeContents(FormFn *fn) {
  if (fn != NULL) {
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

void freeScope(EnvBindingScope *scope);

RetVal tryMakeScope(uint64_t numBindings, EnvBindingScope **ptr, Error *error) {

  RetVal ret;

  EnvBindingScope *scope;
  tryMalloc(scope, sizeof(EnvBindingScope), "EnvBindingScope");

  scope->numBindings = numBindings;
  scope->bindings = NULL;
  scope->next = NULL;

  tryMalloc(scope->bindings, sizeof(EnvBinding) * scope->numBindings, "EnvBinding array");

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
    freeScope(scope);
    return ret;
}

void freeBindingContents(EnvBinding *binding) {
  if (binding != NULL) {
    if (binding->name != NULL) {
      free(binding->name);
      binding->name = NULL;
    }
  }
}

void freeScope(EnvBindingScope *scope) {
  if (scope != NULL) {
    if (scope->bindings != NULL) {
      for (int i=0; i<scope->numBindings; i++) {
        freeBindingContents(scope->bindings + i);
      }
      free(scope->bindings);
    }
    free(scope);
  }
}

void pushScope(EnvBindingStack *stack, EnvBindingScope *scope) {
  scope->next = stack->head;
  stack->head = scope;
  stack->depth = stack->depth + 1;
}

void popScope(EnvBindingStack *stack) {
  EnvBindingScope *scope = stack->head;
  stack->head = scope->next;
  stack->depth = stack->depth - 1;
}

EnvBinding* findBinding(EnvBindingStack *stack, wchar_t *bindingName) {
  EnvBindingScope *scope = stack->head;
  while (scope != NULL) {
    for (int i=0; i<scope->numBindings; i++) {
      if (wcscmp(bindingName, scope->bindings[i].name) == 0) {
        return &scope->bindings[i];
      }
    }
  }
  return NULL;
}


RetVal tryEnvRefAnalyze(FormAnalyzer *analyzer, Expr *expr, EnvBinding *binding, FormEnvRef *envRef, Error *error) {

  envRef->expr = expr;
  envRef->type = binding->type;
  envRef->index = binding->index;

  return R_SUCCESS;
}

Var* resolveVar(FormAnalyzer *analyzer, wchar_t *symbolName, uint64_t symbolNameLength) {

  Namespace *ns;
  wchar_t *searchName;
  uint64_t searchNameLength;

  // assume unqualified namespace at first
  ns = analyzer->currentNamespace;
  searchName = symbolName;
  searchNameLength = symbolNameLength;

  if (symbolNameLength > 2) { // need at least three characters to qualify a var name with a namespace: ('q/n')

    // attempt to find a qualified namespace
    wchar_t *slashPtr = wcschr(symbolName, L'/');
    if (slashPtr != NULL) { // qualified
      uint64_t nsLen = slashPtr - symbolName;

      for (int i=0; i<analyzer->numNamespaces; i++) {

        Namespace *thisNs = &analyzer->namespaces[i];
        if (wcsncmp(symbolName, thisNs->name, nsLen) == 0) {
          ns = thisNs;
          searchName = slashPtr + 1;
          searchNameLength = symbolNameLength - (nsLen + 1);
          break;
        }
      }
    }
  }

  // find var within namespace

  for (int i=0; i<ns->numLocalVars; i++) {
    if (wcsncmp(searchName, ns->localVars[i].name, searchNameLength) == 0) {
      return &ns->localVars[i];
    }
  }

  return NULL;
}

RetVal tryVarRefAnalyze(FormAnalyzer *analyzer, Expr *expr, Var *var, FormVarRef *varRef, Error *error) {

  varRef->var = var;

  return R_SUCCESS;
}

// TODO: need a more sustainable way to create protocols over different kinds of objects

uint64_t getExprPosition(Expr *expr) {
  switch (expr->type) {
    case N_STRING:
      return expr->string.token->position;
    case N_NUMBER:
      return expr->number.token->position;
    case N_SYMBOL:
      return expr->symbol.token->position;
    case N_KEYWORD:
      return expr->keyword.token->position;
    case N_BOOLEAN:
      return expr->boolean.token->position;
    case N_NIL:
      return expr->nil.token->position;
    case N_LIST:
      return expr->list.oParen->position;
    default:
      return 0;
  }
}

uint64_t getFormPosition(Form *form) {
  switch (form->type) {
    case F_CONST:
      return getExprPosition(form->constant);
    case F_IF:
      return getExprPosition(form->iff.expr);
    case F_LET:
      return getExprPosition(form->let.expr);
    case F_ENV_REF:
      return getExprPosition(form->envRef.expr);
    case F_VAR_REF:
      return getExprPosition(form->varRef.symbol);
    case F_FN:
      return getExprPosition(form->fn.expr);
    case F_BUILTIN:
      return getExprPosition(form->builtin.expr);
    case F_FN_CALL:
      return getExprPosition(form->fnCall.expr);
    default:
      return 0;
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
      if (form->varRef.var->value.type != F_FN) {
        throwSyntaxError(error, getFormPosition(form),
                         "var's value does not contain a function: '%ls'", form->varRef.var->name);
      }
      break;

    case F_CONST:
      if (form->constant->type != N_KEYWORD) {
        throwSyntaxError(error, getFormPosition(form),
                         "first list element cannot be called as a function: '%i'", form->constant->type);
      }
      break;
  }

  return R_SUCCESS;

  failure:
    return ret;
}


void fnCallFreeContents(FormFnCall *fnCall);

RetVal tryFnCallAnalyze(FormAnalyzer *analyzer, Expr *expr, FormFnCall *fnCall, Error *error) {

  RetVal ret;

  fnCall->expr = expr;

  throws(tryFormAnalyze(analyzer, expr->list.head->expr, &fnCall->fnCallable, error));
  throws(assertFnCallable(fnCall->fnCallable, error));

  fnCall->numArgs = expr->list.length - 1;
  tryMalloc(fnCall->args, sizeof(Form) * fnCall->numArgs, "Form array");

  ListElement *argExpr = expr->list.head->next;
  for (int i=0; i<fnCall->numArgs; i++) {
    Form *arg = fnCall->args + i;
    throws(tryFormAnalyzeContents(analyzer, argExpr->expr, arg, error));
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

RetVal tryFormAnalyzeContents(FormAnalyzer *analyzer, Expr* expr, Form *form, Error *error) {

  RetVal ret;

  switch (expr->type) {

    // constants
    case N_STRING:
    case N_NUMBER:
    case N_KEYWORD:
    case N_BOOLEAN:
    case N_NIL: {
      form->type = F_CONST;
      form->constant = expr;
      break;
    }

    case N_SYMBOL: {

      wchar_t *sym = expr->symbol.value;
      EnvBinding *envBinding;
      Var *var;

      if ((envBinding = findBinding(&analyzer->bindingStack, sym)) != NULL) {
        throws(tryEnvRefAnalyze(analyzer, expr, envBinding, &form->envRef, error));
      }
      else if ((var = resolveVar(analyzer, sym, expr->symbol.length)) != NULL) {
        throws(tryVarRefAnalyze(analyzer, expr, var, &form->varRef, error));
      }
      else {
        throwSyntaxError(error, expr->symbol.token->position, "cannot resolve symbol: '%ls'", sym);
      }
      break;
    }

    case N_LIST: {

      // empty list
      if (expr->list.length == 0) {
        form->type = F_CONST;
        form->constant = expr;
        break;
      }

      // special forms
      if (expr->list.head->expr.type == N_SYMBOL) {
        wchar_t *sym = expr->list.head->expr->symbol.value;

        if (wcscmp(sym, L"if") == 0) {
          throws(tryIfAnalyze(analyzer, expr, &form->iff, error));
          break;
        }

        if (wcscmp(sym, L"let") == 0) {
          throws(tryLetAnalyze(analyzer, expr, &form->let, error));
          break;
        }

        if (wcscmp(sym, L"fn") == 0) {
          throws(tryFnAnalyze(analyzer, expr, &form->fn, error));
          break;
        }
      }

      // assume fn-callable
      throws(tryFnCallAnalyze(analyzer, expr, &form->fnCall, error));
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

RetVal tryFormAnalyze(FormAnalyzer *analyzer, Expr* expr, Form **ptr, Error *error) {

  RetVal ret;
  Form *form;

  form = malloc(sizeof(Form));
  if (form == NULL) {
    ret = memoryError(error, "Form");
    goto failure;
  }

  throws(tryFormAnalyzeContents(analyzer, expr, form, error));

  *ptr = form;
  return R_SUCCESS;

  failure:
    if (form != NULL) {
      free(form);
    }
    return ret;
}

void formFree(Form* form) {
  if (form != NULL) {
    switch (form->type) {
      case F_CONST:
        // do nothing (FOR NOW)
        break;
      case F_IF:
        ifFreeContents(&form->iff);
        break;
      case F_LET:
        letFreeContents(&form->let);
        break;
      case F_ENV_REF:
        break;
      case F_VAR_REF:
        break;
      case F_FN:
        break;
      case F_BUILTIN:
        break;
      case F_FN_CALL:
        break;
      default:
        // TODO: explode
        break;
    }
    free(form);
  }
}


// TODO: consider a deep copy of input expressions so that you don't have to keep the original input expressions around for freeing later
// seems like it would be tricky to handle freeing properly when two different data structures both point to the same stuff
































