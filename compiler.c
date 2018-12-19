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
 */
