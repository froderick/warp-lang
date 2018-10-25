#include <stdlib.h>
#include "errors.h"
#include "lexer.h"

int main(void) {

  LexerState_t s;

  if (tryLexerStateMake(&s)) {
    goto error;
  }

  Token* t;
  while (1) {
    bool error = tryTokenRead(stdin, s, &t);
    if (error) {
      printf("encountered errors:\n");
      printErrors();
    }
    else {
      printf("token: %ls (%s) %lu %lu\n", t->text, tokenName(t->type), t->position, t->length);
    }
  }
  return 0;

  error:
    printf("encountered terminal errors:\n");
    printErrors();
    exit(-1);
}


