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
  
  more on this. there are really only a few types of errors the lexer handles:
  - memory allocation errors
  - io errors
  - unrecognized input errors
  
  for error reporting to be useful, the caller must be able to distinguish between these three cases
  only in the third case is position in the stream relevant
  
  I could add an enum on the Errors struct to indicate which type of error it is. That would at least
  tell the caller whether or not to look for position/error message info and display it as such.
  
  The response codes of success, eof, and error remain. 
  - success means you got another token
  - eof means you didn't get another token because there aren't any more
  - error means you got a token for some other reason, you can look in the Errors struct for more information
  
  *But, I should probably stop returning eof when I ran into an eof and then completed parsing a valid token.*
  TODO: EOF should mean 'I didn't find a token because the stream is empty'
  
* it pains me to say it, but nowhere am I checking for numeric overflows in 
  my counters. is this a thing I have to do? probably

## Parser TODO
* see what is salvageable from the past
