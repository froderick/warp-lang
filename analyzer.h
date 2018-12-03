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
typedef struct Var Var;

typedef struct FormIf {
  Form *test;
  Form *ifBranch;
  Form *elseBranch;
} FormIf;

typedef struct LexicalBinding {
  Expr *symbol;
  Form *value;
} LexicalBinding;

typedef struct FormLet {
  LexicalBinding *bindings;
  uint64_t numBindings;
  Form *forms;
  uint64_t numForms;
} FormLet;

typedef enum FormEnvRefType {
  RT_NONE,
  RT_ARG,         // function argument
  RT_LOCAL,       // a local binding within a function
  RT_RT_CAPTURED  // a captured variable from the surrounding lexical context
} FormEnvRefType;

typedef struct FormEnvRef {
  FormEnvRefType type;
  uint64_t index;
  Expr *expr;
} FormEnvRef;

typedef struct FormVarRef {
  Var *var;
} FormVarRef;

typedef struct FormFnArg {
  wchar_t *name;
  uint64_t nameLength;
  Expr *expr;
} FormFnArg;

typedef struct FormFn {
  FormFnArg *args;
  uint64_t numArgs;
  Form *forms;
  uint64_t numForms;
} FormFn;

typedef struct FormFnCall {
  Form *fnCallable;
  Form *args;
  uint64_t numArgs;
} FormFnCall;

typedef enum FormBuiltinType {
  B_NONE
} FormBuiltinType;

typedef struct FormBuiltin {
  wchar_t* name;
  Form *args;
  uint64_t numArgs;
} FormBuiltin;

typedef enum FormType {
  F_NONE,
  F_CONST,
  F_IF,
  F_LET,
  F_ENV_REF,
  F_VAR_REF,
  F_FN,
  F_BUILTIN,
  F_FN_CALL
} FormType;

typedef struct Form {
  FormType type;
  union {
    Expr *constant;
    FormIf iff;
    FormLet let;
    FormEnvRef envRef;
    FormVarRef varRef;
    FormFn fn;
    FormFnCall fnCall;
    FormBuiltin builtin;
  };
} Form;

typedef struct Var {
  wchar_t *namespace;
  wchar_t *name;
  Form *value;
} Var;

typedef struct Namespace {
  wchar_t *name;
  Var *localVars;
  uint64_t numLocalVars;
  Var *importedVars;
  uint64_t numImportedVars;

} Namespace;


/*
 * I need a way to track the symbols that are currently in scope, lexically speaking.
 * I could use a stack for this, where each element in the stack is an array of environment
 * bindings. Each binding would contain the symbol name, as well as what. Not sure.
 * But I can start by tracking them and figure that out later.
 */

typedef struct EnvBinding {
  wchar_t *name;
  uint64_t nameLength;
  FormEnvRefType type;
  uint64_t index;
} EnvBinding;

typedef struct EnvBindingScope {
  uint64_t numBindings;
  EnvBinding *bindings;
  struct EnvBindingScope *next;
} EnvBindingScope;

typedef struct EnvBindingStack {
  uint64_t depth;
  EnvBindingScope *head;
} EnvBindingStack;

typedef struct FormAnalyzer {
  Namespace *namespaces;
  uint64_t numNamespaces;
  Namespace *currentNamespace;
  EnvBindingStack bindingStack;
} FormAnalyzer;

RetVal tryAnalyzerMake(FormAnalyzer **ptr, Error *error);
void analyzerFree(FormAnalyzer *analyzer);

RetVal tryFormAnalyze(FormAnalyzer *analyzer, Expr* expr, Form **form, Error *error);
void formFreeContents(Form* expr);
void formFree(Form* expr);


// TODO: when a function definition captures values from its lexical scope, those values are copied from their
// lexical scope into the function object itself. So the compiler needs a way to figure out how to generate
// code that copies these values whenever they are captured.








































