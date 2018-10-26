#include <stdlib.h>
#include "errors.h"
#include "lexer.h"

int main(void) {

  LexerState_t s;

  if (tryLexerStateMake(&s)) {
    goto error;
  }

  Token t;
  while (1) {
    int read = tryTokenRead(stdin, s, &t);
    if (read == LEX_EOF) {
      continue;
    }
    else if (read == LEX_ERROR) {
      printf("encountered errors:\n");
      printErrors();
    }
    else if (read == LEX_SUCCESS) {
      printf("token: %ls (%s) %lu %lu\n", t.text, tokenName(t.type), t.position, t.length);
    }
    else {
      printf("encountered unknown response from lexer: %i\n", read);
    }
  }
  return 0;

  error:
    printf("encountered terminal errors:\n");
    printErrors();
    exit(-1);
}


