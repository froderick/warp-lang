#ifndef WARP_LANG_POOL_H
#define WARP_LANG_POOL_H

#include "../errors.h"

typedef struct Pool *Pool_t;

RetVal tryPoolCreate(Pool_t *pool, uint64_t segmentSize, Error *error);
RetVal tryPoolAllocate(Pool_t pool, void **ptr, size_t size, char *description, Error *error);
uint64_t poolSize(Pool_t pool);
void poolClear(Pool_t pool);
void poolFree(Pool_t pool);

// make it more like tryMalloc
#define tryPalloc(pool, var, size, desc) {\
  throws(tryPoolAllocate(pool, (void*)&var, size, desc, error)); \
}

#define palloc(pool, var, size, desc) {\
  Error error; \
  errorInitContents(&error); \
  if (tryPoolAllocate(pool, (void*)&var, size, desc, &error) != R_SUCCESS) { \
    explode("cannot palloc"); \
  } \
}

#endif //WARP_LANG_POOL_H
