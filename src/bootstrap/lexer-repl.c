#include <stdlib.h>
#include <inttypes.h>
#include "lexer.h"

#define ONE_MB (1024 * 1000)

int main(void) {

  Error e;

  Pool_t pool = poolCreate(ONE_MB);

  InputStream_t source;
  int error = tryFileInputStreamMake(pool, stdin, &source, &e);
  if (error) {
    printf("whoops 1");
  }

  TokenStream_t stream = streamMake(pool, source);

  Token *t;
  while (1) {
    int read = tryStreamNext(pool, stream, &t, &e);
    if (read == R_EOF) {
      continue;
    }
    else if (read == R_ERROR) {
      printf("> encountered lexer error\n\n");
    }
    else if (read == R_SUCCESS) {
      printf("token: %ls (%s) %" PRIu64 " %" PRIu64 "\n", t->text, t->typeName, t->source.position, t->source.length);
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


