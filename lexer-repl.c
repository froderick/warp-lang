#include <stdlib.h>
#include "errors.h"
#include "lexer.h"

int main(void) {
  LexerState_t s = lexerStateMake();
  Token* t;
  while (1) {
    bool error = tokenRead(stdin, s, &t);
    if (error) {
      printf("encountered errors:\n");
      printErrors();
    }
    else {
      printf("token: %ls (%s) %lu %lu\n", t->text, tokenName(t->type), t->position, t->length);
    }
  }
  return 0;
}


