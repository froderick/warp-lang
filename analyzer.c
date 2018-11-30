  #include <stdint.h>
#include <stdlib.h>

#include "analyzer.h"
#include "utils.h"

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

void constantAnalyze(Expr* expr, Form *form) {
  form->type = F_CONST;
  form->constant = expr;
}

void constantFreeContents(Expr *constant) {
    // nothing to do
}

RetVal tryIfAnalyze(FormAnalyzer *analyzer, Expr* ifExpr, Form *form, Error *error) {

  // TODO: why do these functions take generic Forms and not specific ones?

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

  Form *test;
  Form *ifBranch;
  Form *elseBranch;

  ret = tryFormAnalyze(analyzer, testExpr, &test, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  ret = tryFormAnalyze(analyzer, ifBranchExpr, &ifBranch, error);
  if (ret != R_SUCCESS) {
    goto failure;
  }

  if (ifExpr->list.length == 4) {
    ret = tryFormAnalyze(analyzer, elseBranchExpr, &elseBranch, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }
  }

  form->iff.test = test;
  form->iff.ifBranch = ifBranch;
  form->iff.elseBranch = elseBranch;

  return R_SUCCESS;

  failure:
    if (test != NULL) {
      formFree(test);
    }
    if (ifBranch != NULL) {
      formFree(ifBranch);
    }
    if (elseBranch != NULL) {
      formFree(elseBranch);
    }
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

RetVal tryLetAnalyze(FormAnalyzer *analyzer, Expr* letExpr, Form *form, Error *error) {

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

  uint64_t numBindings = bindingsExpr->list.length / 2;

  // things that get cleaned up on failure
  LexicalBinding *bindings;
  Form *bindingValue;
  Form *forms;

  bindings = malloc(sizeof(LexicalBinding) * numBindings);
  if (bindings == NULL) {
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

    bindings[i].symbol = bindingElem->expr;
    bindings[i].value = bindingValue;
    bindingValue = NULL; // now part of bindings

    bindingElem = bindingElem->next->next;
  }

  uint64_t numForms = letExpr->list.length - 2;
  forms = malloc(sizeof(Form) * numForms);
  if (forms == NULL) {
    ret = memoryError(error, "Forms array");
    goto failure;
  }

  Expr *expr = letExpr->list.head->next->next->expr;
  for (int i=0; i<numForms; i++) {
    Form *thisForm = forms + i;
    ret = tryFormAnalyze(analyzer, expr, &thisForm, error);
    if (ret != R_SUCCESS) {
      goto failure;
    }
  }

  form->let.bindings = bindings;
  form->let.numBindings = numBindings;
  form->let.forms = forms;
  form->let.numForms = numForms;

  return R_SUCCESS;

  failure:
    if (bindings != NULL) {
      for (int i=0; i<numBindings; i++) {
        LexicalBinding *b = bindings + i;
        // don't need to free symbol
        formFree(b->value);
      }
      free(bindings);
    }
    if (bindingValue != NULL) {
      formFree(bindingValue);
    }
    if (forms != NULL) {
      for (int i=0; i<numForms; i++) {
        formFreeContents(forms + i);
      }
      free(forms);
    }
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

RetVal tryFnAnalyze(FormAnalyzer *analyzer, Expr* fnExpr, Form *form, Error *error);

RetVal tryFormAnalyze(FormAnalyzer *analyzer, Expr* expr, Form **ptr, Error *error) {

  RetVal ret;
  Form *form;

  form = malloc(sizeof(Form));
  if (form == NULL) {
    ret = memoryError(error, "Form");
    goto failure;
  }

  switch (expr->type) {

    // constants
    case N_STRING:
    case N_NUMBER:
    case N_KEYWORD:
    case N_BOOLEAN:
    case N_NIL: {
      constantAnalyze(expr, form);
      ret = R_SUCCESS;
      break;
    }

    case N_SYMBOL: {
      if (wcscmp(expr->symbol.value, L"if") == 0) {
        ret = tryIfAnalyze(analyzer, expr, form, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }
      else if (wcscmp(expr->symbol.value, L"let") == 0) {
        ret = tryLetAnalyze(analyzer, expr, form, error);
        if (ret != R_SUCCESS) {
          goto failure;
        }
        break;
      }
//      else if (wcscmp(expr->symbol.value, L"fn") == 0) {
//        ret = tryFnAnalyze(analyzer, expr, form, error);
//        if (ret != R_SUCCESS) {
//          goto failure;
//        }
//        break;
//      }
//      else if (isEnvRef()) {
//        analyzeEnvRef();
//      }
//      else if (isVarRef()) {
//        analyzeVarRef();
//      }
      else {
        int len = 64;
        char msg[len];
        snprintf(msg, len, "cannot resolve symbol: '%ls'", expr->symbol.value);
        ret = syntaxError(error, expr->symbol.token->position, msg);
        goto failure;
      }
    }

    case N_LIST: {
      if (expr->list.length == 0) {
        constantAnalyze(expr, form);
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
        constantFreeContents(form->constant);
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
































