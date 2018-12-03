  #include <stdint.h>
#include <stdlib.h>

#include "analyzer.h"
#include "utils.h"

RetVal tryFormAnalyzeContents(FormAnalyzer *analyzer, Expr* expr, Form *form, Error *error);

RetVal tryNamespaceMake(wchar_t *name, uint64_t length, Namespace **ptr , Error *error) {

  Namespace *ns = malloc(sizeof(Namespace));
  if (ns != NULL) {
    return memoryError(error, "Namespace");
  }

  ns->name = NULL;
  ns->importedVars = NULL;
  ns->numImportedVars = 0;
  ns->localVars = NULL;
  ns->numLocalVars = 0;

  if (tryCopyText(name, &ns->name, length, error) != R_SUCCESS) {
    goto failure;
  }

  *ptr = ns;
  return R_SUCCESS;

  failure:
    if (ns != NULL) {
      free(ns);
    }
    return R_ERROR;
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

  FormAnalyzer *analyzer = malloc(sizeof(FormAnalyzer));
  if (analyzer != NULL) {
    return memoryError(error, "FormAnalyzer");
  }

  Namespace *userNs;
  if (tryNamespaceMake(L"user", wcslen(L"user"), &userNs, error) != R_SUCCESS) {
    goto failure;
  }

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
    return R_ERROR;
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

  iff->test = NULL;
  iff->ifBranch = NULL;
  iff->elseBranch = NULL;

  RetVal ret;

  uint64_t pos = ifExpr->list.oParen->position;
  if (ifExpr->list.length < 2) {
    ret = syntaxError(error, pos, "the 'if' special form requires a test");
    goto failure;
  }
  if (ifExpr->list.length < 3) {
    ret = syntaxError(error, pos, "the 'if' special form requires at least one return value");
    goto failure;
  }
  if (ifExpr->list.length > 4) {
    ret = syntaxError(error, pos, "the 'if' special form only supports an 'if' and 'else' branch");
    goto failure;
  }

  Expr *testExpr = ifExpr->list.head->next->expr;
  Expr *ifBranchExpr = ifExpr->list.head->next->next->expr;
  Expr *elseBranchExpr = ifExpr->list.head->next->next->next->expr;

  ret = tryFormAnalyze(analyzer, testExpr, &iff->test, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryFormAnalyze(analyzer, ifBranchExpr, &iff->ifBranch, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (ifExpr->list.length == 4) {
    ret = tryFormAnalyze(analyzer, elseBranchExpr, &iff->elseBranch, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }
  }

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

  let->numBindings = 0;
  let->bindings = NULL;
  let->numForms = 0;
  let->forms = NULL;

  RetVal ret;

  // sanity checking
  uint64_t pos = letExpr->list.oParen->position;
  if (letExpr->list.length < 2) {
    ret = syntaxError(error, pos, "the 'let' special form requires at least one parameter");
    goto failure;
  }
  Expr *bindingsExpr = letExpr->list.head->next->expr;
  if (bindingsExpr->type != N_LIST) {
    ret = syntaxError(error, pos, "the 'let' special form requires the first parameter to be a list");
    goto failure;
  }
  if (bindingsExpr->list.length % 2 != 0) {
    ret = syntaxError(error, pos, "the 'let' special form requires the first parameter to be a list with an even number of arguments");
    goto failure;
  }

  let->numBindings = bindingsExpr->list.length / 2;

  // things that get cleaned up on failure
  Form *bindingValue;

  let->bindings = malloc(sizeof(LexicalBinding) * let->numBindings);
  if (let->bindings == NULL) {
    ret = memoryError(error, "LexicalBinding array");
    goto failure;
  }

  ListElement *bindingElem = bindingsExpr->list.head;
  for (int i=0; i<letExpr->list.length; i += 2) {

    if (bindingElem->expr->type != N_SYMBOL) {
      ret = syntaxError(error, pos, "only symbols can be bound as names");
      goto failure;
    }

    ret = tryFormAnalyze(analyzer, bindingElem->next->expr, &bindingValue, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }

    let->bindings[i].symbol = bindingElem->expr;
    let->bindings[i].value = bindingValue;
    bindingValue = NULL; // now part of bindings

    bindingElem = bindingElem->next->next;
  }

  let->numForms = letExpr->list.length - 2;
  let->forms = malloc(sizeof(Form) * let->numForms);
  if (let->forms == NULL) {
    ret = memoryError(error, "Forms array");
    goto failure;
  }

  Expr *expr = letExpr->list.head->next->next->expr;
  for (int i=0; i<let->numForms; i++) {
    Form *thisForm = let->forms + i;
    ret = tryFormAnalyzeContents(analyzer, expr, thisForm, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }
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

  fn->numForms = 0;
  fn->args = NULL;
  fn->numForms = 0;
  fn->forms = NULL;

  // sanity checking
  uint64_t pos = fnExpr->list.oParen->position;
  if (fnExpr->list.length < 2) {
    ret = syntaxError(error, pos, "the 'fn' special form requires at least one parameter");
    goto failure;
  }
  Expr *argsExpr = fnExpr->list.head->next->expr;
  if (argsExpr->type != N_LIST) {
    ret = syntaxError(error, pos, "the 'fn' special form requires the first parameter to be a list");
    goto failure;
  }

  fn->numArgs = fnExpr->list.length;
  fn->args = malloc(sizeof(FormFnArg) * fn->numArgs);
  if (fn->args == NULL) {
    ret = memoryError(error, "FormFnArg array");
    goto failure;
  }

  ListElement *argElem = argsExpr->list.head;
  for (int i=0; i<fn->numArgs; i++) {

    if (argElem->expr->type != N_SYMBOL) {
      ret = syntaxError(error, pos, "only symbols can be used as function arguments");
      goto failure;
    }

    FormFnArg *arg = fn->args + i;
    arg->expr = argElem.expr;
    arg->nameLength = arg->expr->symbol.length;
    arg->name = NULL;

    ret = tryCopyText(arg->expr->symbol.value, &arg->name, arg->nameLength, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }

    argElem = argElem->next;
  }

  fn->numForms = fnExpr->list.length - 2;
  fn->forms = malloc(sizeof(Form) * fn->numForms);
  if (fn->forms == NULL) {
    ret = memoryError(error, "Forms array");
    goto failure;
  }

  Expr *expr = fnExpr->list.head->next->next->expr;
  for (int i=0; i<fn->numForms; i++) {
    Form *thisForm = fn->forms + i;
    ret = tryFormAnalyzeContents(analyzer, expr, thisForm, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }
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

RetVal tryMakeScope(uint64_t numBindings, Error *error) {

  RetVal ret;

  EnvBindingScope *scope = malloc(sizeof(EnvBindingScope));
  if (scope == NULL) {
    ret = memoryError(error, "EnvBindingScope");
    goto failure;
  }

  scope->numBindings = numBindings;
  scope->bindings = NULL;
  scope->next = NULL;

  scope->bindings = malloc(sizeof(EnvBinding));
  if (scope->bindings == NULL) {
    ret = memoryError(error, "EnvBinding array");
    goto failure;
  }

  failure:
    freeScope(scope);
    return ret;
}

void freeScope(EnvBindingScope *scope) {
  if (scope != NULL) {
    if (scope->bindings != NULL) {
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
      ret = R_SUCCESS;
      break;
    }

    case N_SYMBOL: {
      if (wcscmp(expr->symbol.value, L"if") == 0) {
        ret = tryIfAnalyze(analyzer, expr, &form->iff, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }

      if (wcscmp(expr->symbol.value, L"let") == 0) {
        ret = tryLetAnalyze(analyzer, expr, &form->let, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }

      if (wcscmp(expr->symbol.value, L"fn") == 0) {
        ret = tryFnAnalyze(analyzer, expr, &form->fn, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }

      EnvBinding *envBinding = findBinding(&analyzer->bindingStack, expr->symbol.value);
      if (envBinding != NULL) {
        ret = tryEnvRefAnalyze(analyzer, expr, &form->envRef, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }

      Var *var = resolveVar(analyzer, expr->symbol.value, expr->symbol.length);
      if (var != NULL) {
        ret = tryVarRefAnalyze(analyzer, expr, &form->envRef, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }

      // explode if we can't resolve the symbol
      {
        int len = 64;
        char msg[len];
        snprintf(msg, len, "cannot resolve symbol: '%ls'", expr->symbol.value);
        ret = syntaxError(error, expr->symbol.token->position, msg);
        goto failure;
      }
    }

    case N_LIST: {
      if (expr->list.length == 0) {
        form->type = F_CONST;
        form->constant = expr;
        ret = R_SUCCESS;
        break;
      }
      else {
        Expr* first = expr->list.head->expr;
        switch (first->type) {
          case N_LIST: {
            // it may:
            // - define an fn
            // - call a function that could return an fn
          }
          case N_KEYWORD:
            // - keywords can be called as functions
          case N_SYMBOL:
            // it may:
            // - point to a var, which must then currently contain a function as its value
            // - point to a binding, which may contain a fn definition now which can be enforced, or which may compute a function and return it at runtime
            // - point to a builtin function
            break;
          default:
            break;
            // TODO: explode, this is not a valid fn-callable thing
        }
      }
    }

    default: {
      int len = 64;
      char msg[len];
      snprintf(msg, len, "unhandled expression type: '%i'", expr->type);
      ret = internalError(error, msg);
      goto failure;
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

  ret = tryFormAnalyzeContents(analyzer, expr, form, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

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
































