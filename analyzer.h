#ifndef WARP_LANG_ANALYZER_H
#define WARP_LANG_ANALYZER_H

#include <stdint.h>
#include <stdlib.h>

#include "errors.h"
#include "reader.h"
#include "expander.h"

typedef enum BindingSource {
  BS_NONE,
  BS_CAPTURED,
  BS_LOCAL,
} BindingSource;

typedef enum LocalBindingType {
  BT_NONE,
  BT_LET,
  BT_FN_REF,
  BT_FN_ARG,
} LocalBindingType;

typedef struct LocalBindingInfo {
  LocalBindingType type;
  uint16_t typeIndex; // each type of binding is numbered so it can be referenced
} LocalBindingInfo;

typedef struct CapturedBindingInfo {
  uint16_t bindingIndex;
} CapturedBindingInfo;

typedef struct Binding {
  Text name;
  BindingSource source;
  union {
    LocalBindingInfo local;
    CapturedBindingInfo captured;
  };
} Binding;

typedef struct BindingTable {
  uint16_t allocatedSpace;
  uint16_t usedSpace;
  Binding *bindings;
} BindingTable;

typedef struct Form Form;

typedef struct Forms {
  uint16_t numForms;
  Form *forms;
} Forms;

typedef struct FormIf {
  Form *test;
  Form *ifBranch;
  Form *elseBranch;
} FormIf;

typedef struct LetBinding {
  Text name;
  SourceLocation source;
  Form *value;
  uint16_t bindingIndex;
} LetBinding;

typedef struct FormLet {
  LetBinding *bindings;
  uint16_t numBindings;
  Forms forms;
} FormLet;

typedef struct FormDef {
  Text name;
  Form *value;
} FormDef;

typedef struct FormEnvRef {
  uint64_t bindingIndex; // this is an typeIndex into the binding table for this reference's lexical scope
} FormEnvRef;

typedef struct FormVarRef {
  Text name;
} FormVarRef;

typedef struct FormFnArg {
  Text name;
  SourceLocation source;
  uint16_t bindingIndex;
} FormFnArg;

typedef struct FormFn {

  BindingTable table;

  bool isClosure;
  uint16_t numCaptures;

  // this name is only used within the function to refer to itself, for things like recursion
  bool hasName;
  Text name;
  uint64_t id;
  uint16_t bindingIndex;

  FormFnArg *args;
  uint16_t numArgs;
  Forms forms;

} FormFn;

typedef struct FormFnCall {
  Form *fnCallable;
  Forms args;
  bool tailPosition;
} FormFnCall;

/*
 * `builtin` is a special form that allows code to invoke compile-target specific functionality.
 * Analyzer does not interpret builtins, they are handled exclusively by the compiler/code emitter.
 *
 * (builtin :add 10 20)
 */
typedef struct FormBuiltin {
  Text name;
  Forms args;
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
  };
  SourceLocation source;
} Form;

typedef struct FormRoot {
  BindingTable table;
  Form *form;
} FormRoot;

RetVal tryFormAnalyze(Expr* expr, FormRoot **form, Error *error);

typedef struct AnalyzeOptions {
  Expander_t expander;
} AnalyzeOptions;

void analyzeOptionsInitContents(AnalyzeOptions *options);
RetVal tryFormAnalyzeOptions(AnalyzeOptions options, Expr* expr, FormRoot **ptr, Error *error);

void rootInitContents(FormRoot *root);
void rootFreeContents(FormRoot *root);
void rootFree(FormRoot *root);

#endif //WARP_LANG_ANALYZER_H


// TODO: when a function definition captures values from its lexical scope, those values are copied from their
// lexical scope into the function object itself. So the compiler needs a way to figure out how to generate
// code that copies these values whenever they are captured.

/*
 * TODO: this can be realized by creating a builtin the vm honors to load a reference to the function currently being executed into the op stack
 * - analyzer detects function self-references by keeping a stack of in-scope fn-calls
 * - analyzer creates function reference forms
 * - the compiler emits I_LOAD_FN_REF instructions
 * - the vm honors I_LOAD_FN_REF by keeping a 'current' fn handle in the Frame
 *
 * New Plan
 *
 * - analyzer adds function names as bindings in the binding stack
 *   - these bindings get a new binding type
 *   - bindings of this type are indexed separately from the locals, such that typeIndex represents the function
 *     definition depth
 *
 * - compiler creates constants and emits I_LOAD_CONSTs based on references to these new bindings
 *
 * - vm hydration has two phases now:
 *   - the current phase where things get allocated
 *   - the new phase where the function reference constants are resolved. two phases are needed because with the
 *     current algo hydration is depth-first recursive, and I'm not eager to change it right now.
 *
 */

// TODO: need to support 'ns' special form



/*
 * Here's a question: how does the compiler know how to identify a var?
 *
 * We can store the var name as a constant. But the VarRef needs to have the constant typeIndex for it.
 *
 * Is it important that N references to the same var in the same compilation unit all reference the exact same
 * constant typeIndex when loading the var value? We already do this for locals and arguments.
 *
 * We could, during analysis, give each distinct var its own typeIndex id, and then made each var reference use that
 * typeIndex id, Then, during compilation we could add those as constants. We'd have to either insert these constants
 * first before adding others
 *
 * // TODO: no, this should just be the name
 * // we can compute distinct values here in the compiler. it can keep track of the strings it has seen, and
 * // note which ones have been given a constant typeIndex
 */

// TODO: in the call stack, these fields don't really tell you much about this
// binding other than that it exists, and how it was defined. is this enough?
// --
// also, the captured type isn't terribly helpful, since that isn't a property
// of the environment, but rather a property of the reference itself


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































