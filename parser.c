#include "parser.h"

#include <stdint.h>
#include <stdlib.h>
#include "lexer.h"

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


//  T_NONE,
//  T_OPAREN,
//  T_CPAREN,
//  T_OVEC,
//  T_CVEC,
//  T_OBRACKET,
//  T_CBRACKET,
//  T_TRUE,
//  T_FALSE,
//  T_NIL,
//  T_QUOTE,
//  T_NUMBER,
//  T_STRING,
//  T_SYMBOL,
//  T_KEYWORD


// TODO: need to stretch the error struct to be more general, usable by the entire compiler
// TODO: perhaps it is not too soon to smash the lexer and parser together into the reader, and make the analyzer separate
//int parseExpr(TokenStream_t stream, Error *error) {
//
//  Token *t;
//  int read = tryStreamNext(stream, &t, &error);
//
//  if (read != R_SUCCESS) {
//    // TODO, do this properly
//    return R_ERROR;
//  }
//
//  switch (t->type) {
//    case T_NUMBER:
//    case T_STRING:
//
//    // TODO: the lexer probably doesn't have to do all this work, these are all just symbols
//    case T_SYMBOL:
//    case T_KEYWORD:
//    case T_NIL:
//    case T_QUOTE:
//    case T_FALSE:
//    case T_TRUE:
//      // make atom
//      break;
//
//    case T_OPAREN:
//      // make list
//      break;
//
//    default:
//      // TODO: explode
//      break;
//  }
//
//  return R_SUCCESS; // TODO: need general success error code
//}

int main(int argv, char** argc) {

//  wchar_t* text = L"(one two three)";
//
//  Error e;
//
//  StreamSource_t source;
//  int error = trySourceMakeString(text, wcslen(text), &source, &e);
//  if (error) {
//    printf("whoops 1");
//  }
//
//  TokenStream_t stream;
//  error = tryStreamMake(source, &stream, &e);
//  if (error) {
//    printf("whoops 2");
//  }
//
//  Token *t;
//  int read;
//  while ((read = tryStreamNext(stream, &t, &e)) == R_SUCCESS) {
//    printf("token: %ls (%s) %lu %lu\n", t->text, t->typeName, t->position, t->length);
//    free(t);
//  }
//
//  if (read == R_EOF) {
//    printf("eof\n");
//  }
//  else {
//    printf("error\n");
//  }
//
  return 0;
}

