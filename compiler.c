#include "compiler.h"

/*
 * Thinking about macro expansion
 *
 * I don't want to write an interpreter, as well as a virtual machine and a compiler that compiles to it, just to be
 * able to execute macros at compile time. I'd rather write the compiler/vm and then do incremental compilation to
 * handle executing the macros at compile time.
 *
 * I'd do this by passing in a VM handle to the compiler as a parameter. The compiler can compile all forms, including
 * macros, and load them into the VM as it discovers them. Every form gets macro-expanded as a part of compilation.
 * When the compiler encounters a reference to a macro, it could take the arguments and feed them into a call to the
 * compiled macro inside the VM, and then use the result for compilation.
 *
 * TODO: think about this: one of the benefits of this model is that it allows us to handle resolving Vars with
 * // the actual virtual machine itself, rather than having to duplicate this in the compiler/analyzer itself.
 * // of course, this suggests that the var resolution should perhaps not be done in the analyzer at all...
 * // rather, the VarRef can just be the name of the symbol, unqualified. the compiler can inspect the symbols in
 * // the namespaces and do compile-time resolution based on what it finds in the virtual machine.
 */
