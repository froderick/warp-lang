#include <stdlib.h>
#include "errors.h"
#include "lexer.h"

int main(void) {

  TokenStream_t stream;
  int error = tryStreamMake(stdin, &stream);
  if (error) {
    printf("whoops 1");
  }

  Token *t;
  while (1) {
    int read = tryStreamNext(stream, &t);
    if (read == LEX_EOF) {
      continue;
    }
    else if (read == LEX_ERROR) {
      printf("encountered errors:\n");
      printErrors();
    }
    else if (read == LEX_SUCCESS) {
      printf("token: %ls (%s) %lu %lu\n", t->text, t->typeName, t->position, t->length);
    }
    else {
      printf("encountered unknown response from lexer: %i\n", read);
    }
    free(t);
  }
  return 0;

  error:
    printf("encountered terminal errors:\n");
    printErrors();
    exit(-1);
}


