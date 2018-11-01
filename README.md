## Building

```
brew install check cmake
```

## Notables
- in theory this is how to print stack traces, though it seems to suck: https://spin.atomicobject.com/2013/01/13/exceptions-stack-traces-c/

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
* see what is salvageable from the past
