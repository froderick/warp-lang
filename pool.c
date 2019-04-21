#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "pool.h"
#include "errors.h"

typedef struct Segment Segment;

typedef struct Segment {
  void *data;           // the first valid address in the segment
  void *dataEnd;        // the first address after the end of the segment
  void *allocPtr;       // the offset to use for allocation
  Segment *next;        // the subsequent segment, may be NULL
} Segment;

void segmentInitContents(Segment *segment) {
  segment->data = NULL;
  segment->dataEnd = NULL;
  segment->allocPtr = NULL;
  segment->next = NULL;
}

typedef struct Pool {
  uint64_t segmentSize; // the size of each allocated segment (configuration)
  Segment *root;        // the first segment to be allocated
  Segment *current;     // the most recent segment to be allocated
} Pool;

void poolInitContents(Pool *pool) {
  pool->segmentSize = 0;
  pool->root = NULL;
  pool->current = NULL;
}

RetVal tryPoolCreate(Pool **ptr, uint64_t segmentSize, Error *error) {
  RetVal ret;

  Pool *pool = NULL;
  tryMalloc(pool, sizeof(Pool), "Pool");

  poolInitContents(pool);
  pool->segmentSize = segmentSize;

  *ptr = pool;
  return R_SUCCESS;

  failure:
    return ret;
}

RetVal tryMakeSegment(uint64_t segmentSize, Segment **ptr, Error *error) {
  RetVal ret;

  Segment *segment = NULL;
  tryMalloc(segment, sizeof(Segment), "Segment");
  segmentInitContents(segment);

  tryMalloc(segment->data, segmentSize, "Segment data");
  segment->allocPtr = segment->data;
  segment->dataEnd = segment->data + segmentSize;
  segment->next = NULL;

  *ptr = segment;

  return R_SUCCESS;
  failure:
  return ret;
}

RetVal tryPoolAllocate(Pool *pool, void **ptr, size_t size, char *description, Error *error) {
  RetVal ret;

  if (size == 0) {
//    throwRuntimeError(error, "cannot allocate a size of zero");
    *ptr = NULL;
    return R_SUCCESS;
  }

  if (size > pool->segmentSize) {
    throwRuntimeError(error, "cannot allocate a size bigger than %zu: %s", size, description);
  }

  if (pool->root == NULL) { // create root segment if missing

    Segment *segment = NULL;
    throws(tryMakeSegment(pool->segmentSize, &segment, error));

    pool->root = segment;
    pool->current = segment;
  }
  else if (pool->current->allocPtr + size >= pool->current->dataEnd) { // add new segment

    Segment *segment = NULL;
    throws(tryMakeSegment(pool->segmentSize, &segment, error));

    pool->current->next = segment;
    pool->current = segment;
  }

  *ptr = pool->current->allocPtr;
  pool->current->allocPtr += size;

  return R_SUCCESS;
  failure:
  return ret;
}

uint64_t poolSize(Pool *pool) {

  uint64_t result = 0;

  Segment *cursor = pool->root;
  while (cursor != NULL) {
    result += pool->segmentSize;
    cursor = cursor->next;
  }

  return result;
}

void poolClear(Pool *pool) {

  Segment *cursor = pool->root;
  while (cursor != NULL) {
    free(cursor->data);
    cursor = cursor->next;
  }

  pool->root = NULL;
  pool->current = NULL;
}

void poolFree(Pool *pool) {
  if (pool != NULL) {
    poolClear(pool);
    free(pool);
  }
}
