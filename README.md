## Building

```
brew install check cmake
```

## C Conventions

## Lexer TODO
* EOF should mean 'I didn't find a token because the stream is empty'
* Document error handling strategy and semantics in lexer.h
* do the clang thing where it points out to you in the error message *exactly* 
  where in the three relevant lines+numbers/cols a file couldn't be tokenized 
  properly.
* it pains me to say it, but nowhere am I checking for numeric overflows in 
  my counters. is this a thing I have to do? probably

## Parser TODO
* create the runtime 
  - primitive values (int, float, bool, nil, unicode-char, object)
  - collections
  - functions
  - a symbol lookup table
* create a runtime allocator/gc system
* create a parser that emits an ast 
* compile the ast to an IL (CPS?)
* emit machine code from the IL
* pack that machine code into an ELF along with the statically-linked runtime


* see what is salvageable from the past


## Architectural Decisions

### Runtime
- values are garbage-collected and exist on the heap exclusively and are passed 
  around as references, except for some primitive types that can be represented 
  within the address space of the reference itself (int, bool, nil, etc).
  
- the type of a value is represented by the highest three bits of its reference

- all values support truthiness; only false and nil are falsey

- global variables are stored in vars, which are scoped by namespace and are part 
  of the runtime state. procedure definitions compiled at compile-time are 
  pre-populated in the var table at code-generation-time.
  
- how does the stack work?
  - the stack should have a fixed structure that is predictable and easy to traverse for
    - gc tracing
    - stack trace generation

### Garbage Collection

- this will be a precise, tracing, mark-and-sweep, moving, stop-the-world gc

- the root objects will be:
    - referenced by vars
    - referenced by the stack
    
- an object that is allocated must be moved into a root position from registers
  _before_ a gc happens, so that my previous mistake is not repeated where 
  intermediate allocations mid-allocation break things.
  
- allocation must be very fast and require almost no instructions at all, likely
  the pointer to the gc memory belongs in a dedicated register a la OCaml.
  
### Continuations
These are really interesting, but I've concluded they can be added after the fact.
Even when continuations are supported, unless you actually create one the generated
code is essentially the same as without continuation support.

https://www.cs.indiana.edu/~dyb/papers/3imp.pdf

## Misc Docs

*TODO* - Here follows a piece of text that seems maybe useful when writing 
documentation for the reader/analyzer, though isn't complete. I just need it 
out of the code.

```
// This parser takes in a stream of tokens and emits an ast.
//
// The parser first creates a basic AST that represents a hierarchical model
// of the parsed tokens. Basic semantic validity is enforced, such as matching
// parens, etc. Syntax errors are reported back in a way that can be helpfully
// rendered to the user via an Errors struct.
//
// Next, the parser runs an analysis phase attempting to validate the semantics
// of the program. It tracks symbol references, procedure and var definitions,
// and function calls. It also rewrites reader syntax to reflect calls to
// builtin procedures. The resulting ast represents valid language constructs.
// Semantic errors are reported back in a way that can be helpfully rendered
// to the user via an Errors struct.
```


## More TODOs
- implement macroexpand:
    ```
    +    printf("macroexpand occurred {\n    ");
    +    throws(tryExprPrn(expr, error));
    +    printf("\n    =>\n    ");
    +    throws(tryExprPrn(&output.result, error));
    +    printf("\n}\n");
    ```

- bump arena allocators would greatly simplify de-allocation/cleaning up after compiler toolchain runs

## some things needed to build on linux, or in linux containers
        apt-get install cmake clang check pkg-config gdbserver
        docker run -it --mount src="$(pwd)/../",target=/warp,type=bind ubuntu
        https://github.com/shuhaoliu/docker-clion-dev



https://matt.sh/howto-c
