#ifndef WARP_LANG_HEAP_H
#define WARP_LANG_HEAP_H

#include <stdint.h>
#include "vm.h"

typedef struct GC {

  // the total memory allocated
  uint64_t heapMemorySize;
  void *heapMemory;

  // the actual heaps
  uint64_t heapSize;
  void *heapA; // the first half of the memory
  void *heapB; // the second half of the memory

  // the current heap
  void *currentHeap; // the heap to use for allocation
  void *currentHeapEnd; // the heap to use for allocation
  void *allocPtr;    // the offset within the heap to use for allocation

} GC;

void GCFreeContents(GC *gc);
void GCInitContents(GC *gc);
void GCCreate(GC *gc, uint64_t maxHeapSize);
void collect(VM_t vm);
void* alloc(VM_t vm, uint64_t length);
void* deref(VM_t vm, Value value);
void relocate(VM_t vm, Value *valuePtr);
void collect(VM_t vm);

typedef void (*RelocateChildren)(VM_t vm, void *obj);

#endif //WARP_LANG_HEAP_H
