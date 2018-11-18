#include "parser.h"

#include <stdint.h>
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

struct AstNode;

typedef struct AstAtom {
  Token* atom;
} AstAtom;

typedef struct AstList {
  Token* oParen;
  Token* cParen;
  uint64_t length;
  struct AstNode** elements;
} AstList;

typedef enum AstNodeType {
  N_NONE,
  N_ATOM,
  N_LIST,
  N_VECTOR,
  N_MAP,
  N_SET
} AstNodeType;

typedef struct AstNode {
  AstNodeType type;
  union {
    AstAtom atom;
    AstList list;
  };
} AstNode;

//int main(int argv, char** argc) {
//
//  LexerError e;
//
//  TokenStream_t stream;
//  int error = tryStreamMake(stdin, &stream, &e);
//  if (error) {
//    printf("whoops 1");
//  }
//
//  Token *t;
//  while (1) {
//    int read = tryStreamNext(stream, &t, &e);
//    if (read == LEX_EOF) {
//      continue;
//    }
//    else if (read == LEX_ERROR) {
//      printf("> encountered lexer error\n\n");
//    }
//    else if (read == LEX_SUCCESS) {
//      printf("token: %ls (%s) %lu %lu\n", t->text, t->typeName, t->position, t->length);
//    }
//    else {
//      printf("encountered unknown response from lexer: %i\n", read);
//    }
//    free(t);
//  }
//  return 0;
//}

