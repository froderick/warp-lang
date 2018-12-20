#include <stdlib.h>
#include "lexer.h"

int main(void) {

  Error e;

  InputStream_t source;
  int error = tryFileInputStreamMake(stdin, &source, &e);
  if (error) {
    printf("whoops 1");
  }

  TokenStream_t stream;
  error = tryStreamMake(source, &stream, &e);
  if (error) {
    printf("whoops 2");
  }

  Token *t;
  while (1) {
    int read = tryStreamNext(stream, &t, &e);
    if (read == R_EOF) {
      continue;
    }
    else if (read == R_ERROR) {
      printf("> encountered lexer error\n\n");
    }
    else if (read == R_SUCCESS) {
      printf("token: %ls (%s) %llu %llu\n", t->text, t->typeName, t->source.position, t->source.length);
    }
    else {
      printf("encountered unknown response from lexer: %i\n", read);
    }
    free(t);
  }
  return 0;

  error:
    printf("encountered terminal errors\n");
    exit(-1);
}


