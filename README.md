## Building

```
brew install check cmake
```

## Notables
- in theory this is how to print stack traces, though it seems to suck: https://spin.atomicobject.com/2013/01/13/exceptions-stack-traces-c/

## C Conventions

### Error Handling

The basic means of indicating an error is to return a bool value from a 
function which may fail which is true in an error state and false otherwise.

Metadata about the error should be recorded by calling `reportError()` (in 
`errors.h`) to push information about the error onto the error stack. 
Functions can inspect the stack with the behavior defined in `errors.h`. 

Functions should clear the stack when errors have been appropriately handled.

Note that the max stack size is fixed. After its limit of unhandled errors has
been reached (16) it will begin to drop old errors to make room for new ones.

## Lexer TODO
* lexer errors are reported via printf and return status codes, but the caller 
  of the lexer has no way to inspect the structure of the lexical errors. at 
  minimum there should be a way to get a handle on specifically which line/col
  cannot be tokenized.
  
* it pains me to say it, but nowhere am I checking for numeric overflows in 
  my counters. is this a thing I have to do? probably

## Parser TODO
* see what is salvageable from the past
