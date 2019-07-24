#include <stdlib.h>
#include "lexer.h"

int main(void) {

  LexerState_t s = makeLexerState();
  int err;
  Token* t;
  while (1) {
    t = readToken(stdin, s, &err);
    if (t == NULL) {
      printf("err: %i\n", err);
    }
    else {
      printf("token: %ls (%s) %lu %lu\n", t->text, tokenName(t->type), t->position, t->length);
    }
  }
  return 0;
}


