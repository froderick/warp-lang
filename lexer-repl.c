#include <stdlib.h>
#include "lexer.h"

int main(void) {

  Error e;

  StreamSource_t source;
  int error = trySourceMakeFile(stdin, &source, &e);
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
    if (read == RET_TOKEN_STREAM_EOF) {
      continue;
    }
    else if (read == RET_ERROR) {
      printf("> encountered lexer error\n\n");
    }
    else if (read == RET_SUCCESS) {
      printf("token: %ls (%s) %lu %lu\n", t->text, t->typeName, t->position, t->length);
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


