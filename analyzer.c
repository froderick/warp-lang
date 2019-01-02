#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "analyzer.h"
#include "utils.h"

/*
 * Core analyzer behavior.
 */

void varFree(Var *var);
RetVal tryFormDeepCopy(Form *from, Form **to, Error *error);

RetVal tryVarInit(wchar_t *namespace, wchar_t *name, Var *var, Error *error) {
  RetVal ret;

  var->namespace = NULL;
  var->name = NULL;
  var->value = NULL;

  throws(tryCopyText(namespace, &var->namespace, wcslen(namespace), error));
  throws(tryCopyText(name, &var->name, wcslen(name), error));

  return R_SUCCESS;

  failure:
    varFree(var);
    return ret;
}

void varFreeContents(Var *var) {
  if (var != NULL) {
    if (var->namespace != NULL) {
      free(var->namespace);
      var->namespace = NULL;
    }
    if (var->name != NULL) {
      free(var->name);
      var->name = NULL;
    }
    if (var->value != NULL) {
      formFree(var->value);
      var->value = NULL;
    }
  }
}

void varFree(Var *var) {
  if (var != NULL) {
    varFreeContents(var);
    free(var);
  }
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

  for (int i=0; i<ns->localVars.length; i++) {
    if (wcsncmp(searchName, ns->localVars.vars[i].name, searchNameLength) == 0) {
      return &ns->localVars.vars[i];
    }
  }

  return NULL;
}

void varListInit(VarList *list) {
  list->length = 0;
  list->allocatedLength = 0;
  list->vars = NULL;
}

void varListFreeContents(VarList *list) {
  if (list != NULL) {
    if (list->vars != NULL) {
      for (int i=0; i<list->length; i++) {
        varFreeContents(&list->vars[i]);
      }
      free(list->vars);
      list->vars = NULL;
      list->length = 0;
      list->allocatedLength = 0;
    }
  }
}

RetVal tryAppendVar(VarList *list, Var var, Error* error) {

  RetVal ret;

  if (list->vars == NULL) {
    uint64_t len = 16;
    tryMalloc(list->vars, len * sizeof(Var), "Var array");
    list->allocatedLength = len;
  }
  else if (list->length == list->allocatedLength) {
    uint64_t newAllocatedLength = list->allocatedLength * 2;

    Var* resizedVars = realloc(list->vars, newAllocatedLength * sizeof(Var));
    if (resizedVars == NULL) {
      ret = memoryError(error, "realloc Var array");
      goto failure;
    }

    list->allocatedLength = newAllocatedLength;
    list->vars = resizedVars;
  }

  list->vars[list->length] = var;
  list->length = list->length + 1;

  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryDefVar(FormAnalyzer *analyzer, wchar_t *symbolName, uint64_t symbolNameLength, Form *value, Error *error) {
  RetVal ret;

  // these get cleaned up on failure
  Var createdVar;
  Form *copiedValue;

  Var *resolvedVar = resolveVar(analyzer, symbolName, symbolNameLength);
  if (resolvedVar == NULL) {
    Namespace *ns = analyzer->currentNamespace;
    throws(tryVarInit(ns->name, symbolName, &createdVar, error));
    throws(tryFormDeepCopy(value, &createdVar.value, error));
    throws(tryAppendVar(&ns->localVars, createdVar, error));
  }
  else if (resolvedVar->value == NULL) {
    throws(tryFormDeepCopy(value, &resolvedVar->value, error));
  }
  else {
    throws(tryFormDeepCopy(value, &copiedValue, error));
    formFree(resolvedVar->value);
    resolvedVar->value = copiedValue;
    copiedValue = NULL; // now part of resolvedVar
  }

  return R_SUCCESS;

  failure:
    varFreeContents(&createdVar);
    if (copiedValue != NULL) {
      formFree(copiedValue);
    }
    return ret;
}


void scopeFree(EnvBindingScope *scope);

RetVal tryScopeMake(uint64_t numBindings, EnvBindingScope **ptr, Error *error) {

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

// TODO: manage the scopes as the analysis is happening

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

RetVal tryNamespaceMake(wchar_t *name, uint64_t length, Namespace **ptr , Error *error) {

  RetVal ret;

  Namespace *ns;
  tryMalloc(ns, sizeof(Namespace), "Namespace");

  ns->name = NULL;
  ns->importedVars = NULL;
  ns->numImportedVars = 0;

  varListInit(&ns->localVars);

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
    varListFreeContents(&ns->localVars);
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

uint64_t getExprPosition(Expr *expr) {
  return expr->source.position;
}

uint64_t getFormPosition(Form *form) {
  return form->source.position;
}

/*
 * Analyzers for the different types of forms.
 */

RetVal tryFormAnalyzeContents(FormAnalyzer *analyzer, Expr* expr, Form *form, Error *error);

void ifFreeContents(FormIf *iff);

RetVal tryIfAnalyze(FormAnalyzer *analyzer, Expr* ifExpr, FormIf *iff, Error *error) {

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

void letFreeContents(FormLet *let);

RetVal tryLetAnalyze(FormAnalyzer *analyzer, Expr* letExpr, FormLet *let, Error *error) {

  let->numBindings = 0;
  let->bindings = NULL;
  let->numForms = 0;
  let->forms = NULL;

  RetVal ret;

  // things that get cleaned up on failure
  EnvBindingScope *scope = NULL;
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

  ListElement *bindingElem = bindingsExpr->list.head;
  for (int i=0; i + 2 < letExpr->list.length; i += 2) {

    if (bindingElem->expr->type != N_SYMBOL) {
      throwSyntaxError(error, pos, "only symbols can be bound as names");
    }

    LexicalBinding *b = let->bindings + i;
    b->nameLength = bindingElem->expr->symbol.length;
    b->source = bindingElem->expr->source;

    throws(tryCopyText(bindingElem->expr->symbol.value, &b->name, b->nameLength, error));
    throws(tryFormAnalyze(analyzer, bindingElem->next->expr, &b->value, error));

    bindingElem = bindingElem->next->next;
  }

  // register the bindings in the environment stack
  throws(tryScopeMake(let->numBindings, &scope, error));

  for (uint64_t i=0; i<let->numBindings; i++) {
    scope->bindings[i].nameLength = let->bindings[i].nameLength;
    throws(tryCopyText(let->bindings[i].name, &scope->bindings[i].name, scope->bindings[i].nameLength, error));
    scope->bindings[i].type = RT_LOCAL;
    scope->bindings[i].index = i;
  }

  pushScope(&analyzer->bindingStack, scope);
  scopePushed = true;

  // create the forms within this lexical scope
  let->numForms = letExpr->list.length - 2;
  tryMalloc(let->forms, sizeof(Form) * let->numForms, "Forms array");

  Expr *expr = letExpr->list.head->next->next->expr;
  for (int i=0; i<let->numForms; i++) {
    Form *thisForm = let->forms + i;
    throws(tryFormAnalyzeContents(analyzer, expr, thisForm, error));
  }

  // discard the registered bindings from the environment stack
  popScope(&analyzer->bindingStack);
  scopeFree(scope);

  return R_SUCCESS;

  failure:
    letFreeContents(let);
    if (scope != NULL) {
      scopeFree(scope);
    }
    if (scopePushed) {
      popScope(&analyzer->bindingStack);
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

RetVal tryDefAnalyze(FormAnalyzer *analyzer, Expr* defExpr, FormDef *def, Error *error) {

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
    throws(tryFormAnalyze(analyzer, defExpr->list.head->next->next->expr, &def->value, error));
  }

  // update the analyzer state so it knows about this def
  throws(tryDefVar(analyzer, def->name, def->nameLength, def->value, error));

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

RetVal tryFnAnalyze(FormAnalyzer *analyzer, Expr* fnExpr, FormFn *fn, Error *error) {

  RetVal ret;

  // things that get cleaned up on failure
  EnvBindingScope *scope = NULL;
  bool scopePushed = false;

  fn->numForms = 0;
  fn->args = NULL;
  fn->numForms = 0;
  fn->forms = NULL;

  // sanity checking
  uint64_t pos = getExprPosition(fnExpr);
  if (fnExpr->list.length < 2) {
    throwSyntaxError(error, pos, "the 'fn' special form requires at least one parameter");
  }
  Expr *argsExpr = fnExpr->list.head->next->expr;
  if (argsExpr->type != N_LIST) {
    throwSyntaxError(error, pos, "the 'fn' special form requires the first parameter to be a list");
  }

  // create the arguments
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

  // register the arguments in the environment stack
  throws(tryScopeMake(fn->numArgs, &scope, error));

  for (uint64_t i=0; i<fn->numArgs; i++) {
    scope->bindings[i].nameLength = fn->args[i].nameLength;
    throws(tryCopyText(fn->args[i].name, &scope->bindings[i].name, scope->bindings[i].nameLength, error));
    scope->bindings[i].type = RT_ARG;
    scope->bindings[i].index = i;
  }

  pushScope(&analyzer->bindingStack, scope);
  scopePushed = true;

  // create the forms within this lexical scope
  fn->numForms = fnExpr->list.length - 2;
  tryMalloc(fn->forms, sizeof(Form) * fn->numForms, "Forms array");

  Expr *expr = fnExpr->list.head->next->next->expr;
  for (int i=0; i<fn->numForms; i++) {
    Form *thisForm = fn->forms + i;
    throws(tryFormAnalyzeContents(analyzer, expr, thisForm, error));
  }

  // discard the registered arguments from the environment stack
  popScope(&analyzer->bindingStack);
  scopeFree(scope);

  return R_SUCCESS;

  failure:
    fnFreeContents(fn);
    if (scope != NULL) {
      scopeFree(scope);
    }
    if (scopePushed) {
      popScope(&analyzer->bindingStack);
    }
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

RetVal tryEnvRefAnalyze(FormAnalyzer *analyzer, Expr *expr, EnvBinding *binding, FormEnvRef *envRef, Error *error) {

  envRef->type = binding->type;
  envRef->index = binding->index;

  return R_SUCCESS;
}

RetVal tryVarRefAnalyze(FormAnalyzer *analyzer, Expr *expr, Var *var, FormVarRef *varRef, Error *error) {

  varRef->var = var;

  return R_SUCCESS;
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
      if (form->varRef.var->value->type != F_FN) {
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

    default:
      throwInternalError(error, "unhandled case %i", form->type);
  }

  return R_SUCCESS;

  failure:
    return ret;
}

void fnCallFreeContents(FormFnCall *fnCall);

RetVal tryFnCallAnalyze(FormAnalyzer *analyzer, Expr *expr, FormFnCall *fnCall, Error *error) {

  RetVal ret;

  fnCall->fnCallable = NULL;
  fnCall->args = NULL;

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

RetVal tryFormAnalyzeContents(FormAnalyzer *analyzer, Expr* expr, Form *form, Error *error) {

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
      Var *var;

      if ((envBinding = findBinding(&analyzer->bindingStack, sym)) != NULL) {
        form->type = F_ENV_REF;
        throws(tryEnvRefAnalyze(analyzer, expr, envBinding, &form->envRef, error));
      }
      else if ((var = resolveVar(analyzer, sym, expr->symbol.length)) != NULL) {
        form->type = F_VAR_REF;
        throws(tryVarRefAnalyze(analyzer, expr, var, &form->varRef, error));
      }
      else {
        throwSyntaxError(error, getExprPosition(expr), "cannot resolve symbol: '%ls'", sym);
      }
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
          throws(tryIfAnalyze(analyzer, expr, &form->iff, error));
          break;
        }

        if (wcscmp(sym, L"let") == 0) {
          form->type = F_LET;
          throws(tryLetAnalyze(analyzer, expr, &form->let, error));
          break;
        }

        if (wcscmp(sym, L"def") == 0) {
          form->type = F_DEF;
          throws(tryDefAnalyze(analyzer, expr, &form->def, error));
          break;
        }

        if (wcscmp(sym, L"fn") == 0) {
          form->type = F_FN;
          throws(tryFnAnalyze(analyzer, expr, &form->fn, error));
          break;
        }
      }

      // assume fn-callable
      form->type = F_FN_CALL;
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

  tryMalloc(form, sizeof(Form), "Form");
  throws(tryFormAnalyzeContents(analyzer, expr, form, error));

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
        // do nothing (FOR NOW)
        break;
      case F_VAR_REF:
        // do nothing (FOR NOW)
        break;
      case F_FN:
        fnFreeContents(&form->fn);
        break;
      case F_BUILTIN:
        // TODO
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
      to->varRef.var = from->varRef.var; // var references don't get deep copied
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
      // TODO
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


