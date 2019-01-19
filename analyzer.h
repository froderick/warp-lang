#ifndef WARP_LANG_ANALYZER_H
#define WARP_LANG_ANALYZER_H

#include <stdint.h>
#include <stdlib.h>

#include "errors.h"
#include "reader.h"

/*
 * What the analyzer needs to do:
 *
 * Take in a sequence of expressions from the reader.
 * Determine whether the expressions are valid. Report clear, semantic errors when they are not.
 * Return an enhanced sequence of expressions that can be used to generate virtual machine code.
 *
 * Each expression will be one of the following:
 * - an evaluation whose resulting value is ignored
 * - an ns declaration, which changes how the analyzer interprets subsequent expressions
 * - a define, which adds/modifies var bindings
 * - a sequence of macro expansions that result in one of the other options
 *
 * The special 'ns' form will tell the analyzer what namespace to use to interpret subsequent
 * expressions it encounters. 'ns' can be called repeatedly. By default the namespace is 'user'.
 *
 * All symbols not explicitly namespaced will be tagged with the namespace they appear in.
 *
 * The non-quoted symbols of each expression will be matched with vars imported into the
 * current namespace or environment bindings defined as part of their lexical scope.
 *
 * 'define' adds/modifies var bindings within a namespace. A var is a reference to the
 * evaluated value. A var's value can be changed through repeated calls to 'define' for the
 * same var name.
 *
 * Values
 * ------
 *
 * Values either
 * - express a constant value (string, number, boolean, nil, syntax-quoted symbol, lambda)
 * - refer to a binding from the environment (symbol referencing lexical binding or var)
 *
 * Some symbols represent special forms, and are handled exceptionally:
 *
 * 'if' provides conditional branching.
 * 'let' permits lexically-scoped binding
 * 'fn' creates an anonymous function (a lambda) as a value, which may also capture bindings
 *      from the environment where it is defined (a closure)
 *
 * 'loop' permits general-purpose iteration
 * 'recur' permits iteration through tail recursion, also used by 'loop'
 *
 * Function Application
 * --------------------
 *
 *  Besides the special forms, the first element of any list will be assumed to be an object
 *  that can be invoked as a function, with the rest of the elements in the list interpreted
 *  to be that function's arguments.
 *  - symbols that are bound to vars can resolve to functions
 *  - symbols that are bound to environment bindings can resolve to functions
 *  - some constants (like keywords and collections) can resolve to functions
 *  - lambdas resolve to functions
 *
 *  Builtin Functions
 *  -----------------
 *
 *  Some functions, though not special forms, are not implemented natively in the language. These
 *  are called builtins, and they are needed to implement the internals of the language. These are
 *  first-class functions from the language's perspective.
 *
 */

typedef struct Form Form;

typedef struct FormIf {
  Form *test;
  Form *ifBranch;
  Form *elseBranch;
} FormIf;

typedef struct LexicalBinding {
  wchar_t *name;
  uint64_t nameLength;
  SourceLocation source;
  Form *value;
  uint16_t index;
} LexicalBinding;

typedef struct FormLet {
  LexicalBinding *bindings;
  uint16_t numBindings;
  Form *forms;
  uint16_t numForms;
} FormLet;

typedef struct FormDef {
  wchar_t *name;
  uint64_t nameLength;
  Form *value;
} FormDef;

typedef enum FormEnvRefType {
  RT_NONE,
  RT_ARG,         // function argument
  RT_LOCAL,       // a local binding within a function
  RT_RT_CAPTURED  // a captured variable from the surrounding lexical context
} FormEnvRefType;

// TODO: in the call stack, these fields don't really tell you much about this
// binding other than that it exists, and how it was defined. is this enough?
// --
// also, the captured type isn't terribly helpful, since that isn't a property
// of the environment, but rather a property of the reference itself
typedef struct FormEnvRef {
  FormEnvRefType type;
  uint64_t index;
} FormEnvRef;

typedef struct FormVarRef {
  uint64_t nameLength;
  wchar_t *name;
  // TODO: make this just the symbol name, no Var reference here... this needs to get moved into the compiler/vm

  /*
   * Here's a question: how does the compiler know how to identify a var?
   *
   * We can store the var name as a constant. But the VarRef needs to have the constant index for it.
   *
   * Is it important that N references to the same var in the same compilation unit all reference the exact same
   * constant index when loading the var value? We already do this for locals and arguments.
   *
   * We could, during analysis, give each distinct var its own index id, and then made each var reference use that
   * index id, Then, during compilation we could add those as constants. We'd have to either insert these constants
   * first before adding others
   *
   * // TODO: no, this should just be the name
   * // we can compute distinct values here in the compiler. it can keep track of the strings it has seen, and
   * // note which ones have been given a constant index
   */
} FormVarRef;

typedef struct FormFnArg {
  wchar_t *name;
  uint64_t nameLength;
  SourceLocation source;
} FormFnArg;

typedef struct FormFn {
  FormFnArg *args;
  uint16_t numArgs;
  Form *forms;
  uint16_t numForms;

  // TODO: things to consider implementing in the analysis phase
  uint64_t numLocals;
  bool tailRecursive;

} FormFn;

typedef struct FormFnCall {
  Form *fnCallable;
  Form *args;
  uint64_t numArgs;
} FormFnCall;

/*
 * `builtin` is a special form that allows code to invoke compile-target specific functionality.
 * Analyzer does not interpret builtins, they are handled exclusively by the compiler/code emitter.
 *
 * (builtin :add 10 20)
 */
typedef struct FormBuiltin {
  wchar_t *name;
  uint64_t nameLength;
  Form *args;
  uint64_t numArgs;
} FormBuiltin;

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
    // TODO: need to support 'ns' special form
  };
  SourceLocation source;
} Form;

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
} EnvBindingScope;

typedef struct EnvBindingStack {
  uint64_t depth;
  EnvBindingScope *head;
} EnvBindingStack;

void envBindingStackInit(EnvBindingStack *bindingStack);
void envBindingStackFreeContents(EnvBindingStack *bindingStack);

RetVal tryFormAnalyze(EnvBindingStack *bindingStack, Expr* expr, Form **form, Error *error);
void formFreeContents(Form* expr);
void formFree(Form* expr);


// TODO: when a function definition captures values from its lexical scope, those values are copied from their
// lexical scope into the function object itself. So the compiler needs a way to figure out how to generate
// code that copies these values whenever they are captured.

RetVal formPrn(Form* form, FILE *file, Error *error);

#endif //WARP_LANG_ANALYZER_H







































