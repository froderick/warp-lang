#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include "../errors.h"
#include "internal.h"
#include "frame.h"

/*
 * alloc/gc impl
 * super useful: http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm
 */

void GCFreeContents(GC *gc) {
  free(gc->heapMemory);
  gc->heapMemory = NULL;
  gc->heapA = NULL;
  gc->heapB = NULL;
  gc->currentHeap = NULL;
  gc->allocPtr = NULL;
}

void GCInitContents(GC *gc) {
  gc->heapMemorySize = 0;
  gc->heapMemory = NULL;
  gc->heapSize = 0;
  gc->heapA = NULL;
  gc->heapB = NULL;
  gc->currentHeap = NULL;
  gc->allocPtr = NULL;
}

void GCCreate(GC *gc, uint64_t maxHeapSize) {
  GCInitContents(gc);

  gc->heapSize = maxHeapSize;
  gc->heapMemorySize = gc->heapSize * 2;

  gc->heapMemory = malloc(gc->heapMemorySize);
  if (gc->heapMemory == NULL) {
    explode("failed to allocate memory for GC");
  }

  memset(gc->heapMemory, 0, gc->heapMemorySize);

  gc->heapA = gc->heapMemory;
  gc->heapB = gc->heapA + gc->heapSize;
  gc->currentHeap = gc->heapA;
  gc->currentHeapEnd = gc->currentHeap + gc->heapSize;
  gc->allocPtr = gc->currentHeap;
}

void collect(VM *vm);

#define R_OOM 1

/*
 * Allocates, returns R_OOM if allocation fails. Doesn't attempt collection.
 */
static int _alloc(GC *gc, uint64_t length, void **ptr) {

  if (length < sizeof(ObjectHeader)) {
    explode("alloc length is too small, %" PRIu64, length);
  }

  if ((length & W_PTR_MASK) != 0) {
    explode("oops, allocation was not 4-byte padded %" PRIu64, length);
  }

  if (gc->allocPtr + length < gc->currentHeapEnd) {
    *ptr = gc->allocPtr;
    gc->allocPtr += length;
    return R_SUCCESS;
  }
  else {
    return R_OOM;
  }
}

/*
 * Allocates, attempts collection if allocation fails.
 */
void* alloc(VM *vm, uint64_t length) {

  void *ptr = NULL;

  if (!vm->config.gcOnAlloc) {
    int success = _alloc(&vm->gc, length, &ptr);
    if (success == R_OOM) {
      collect(vm);
      success = _alloc(&vm->gc, length, &ptr);
      if (success == R_OOM) {
        explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
      }
    }
  }
  else {
    collect(vm);
    int success = _alloc(&vm->gc, length, &ptr);
    if (success == R_OOM) {
      explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

  return ptr;
}

// keeping this *for now*, it makes things slower but safer
void* deref(VM *vm, Value value) {
  GC *gc = &vm->gc;
  void *ptr = (void*)value;
  if (ptr < gc->currentHeap || ptr >= gc->allocPtr) {
    explode("invalid memory address: %p", ptr);
  }
  return ptr;
}

void relocate(VM *vm, Value *valuePtr) {

  Value value = *valuePtr;

  void* ptr = (void*)value;
  if (ptr >= vm->gc.currentHeap && ptr < (vm->gc.currentHeap + vm->gc.heapSize)) {
    explode("cannot relocate from current heap");
  }

  if ((value & W_PTR_MASK) != 0) {
    // only relocate heap objects
    return;
  }

  ObjectHeader *header = (ObjectHeader*)value;
  if ((*header) & W_GC_FORWARDING_BIT) {
    *valuePtr = (*header << 1u);
  }
  else {
    uint64_t size = objectHeaderSizeBytes(*header);

    void *newPtr = NULL;
    if (_alloc(&vm->gc, size, &newPtr) == R_OOM) {
      explode("out of memory, cannot allocate %" PRIu64 " bytes mid-gc", size);
    }

    memcpy(newPtr, (void*)value, size); // this must somehow be pointing to the CFn + address range

    *valuePtr = (Value)newPtr;
    *header = W_GC_FORWARDING_BIT | ((Value)newPtr >> 1u);
  }

}

uint64_t now() {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC_RAW, &now);
  uint64_t millis = now.tv_nsec / 1000000;
  return millis;
}

static void _relocateTable(VM *vm, Table *table) {
  for (uint64_t i=0; i<table->numAllocatedEntries; i++) {
    TableEntry *entry = &table->entries[i];
    if (entry->used) {
      relocate(vm, &(entry->name));
      relocate(vm, &(entry->value));
    }
  }
}

static void _relocateChildren(VM *vm, ValueType type, void *obj) {
  RelocateChildren relocate = vm->valueTypeTable.valueTypes[type].relocateChildren;
  if (relocate != NULL) {
    relocate(vm, obj);
  }
}

void collect(VM *vm) {

  uint64_t oldHeapUsed = vm->gc.allocPtr - vm->gc.currentHeap;

  uint64_t start = now();
  printf("gc: starting, %" PRIu64 " bytes used\n", oldHeapUsed);

  // flip heaps
  void *oldHeap = vm->gc.currentHeap;
  if (oldHeap == vm->gc.heapA) {
    vm->gc.currentHeap = vm->gc.heapB;
  }
  else {
    vm->gc.currentHeap = vm->gc.heapA;
  }
  vm->gc.currentHeapEnd = vm->gc.currentHeap + vm->gc.heapSize;
  vm->gc.allocPtr = vm->gc.currentHeap;

  memset(vm->gc.currentHeap, 0, vm->gc.heapSize); // TODO: this seems to clobber '+'

  // relocate exception, if present
  relocate(vm, &vm->exception);

  // relocate tables
  _relocateTable(vm, &vm->symbolTable);
  _relocateTable(vm, &vm->keywordTable);

  // relocate noFrameRoots
  FrameRoot_t noFrameRoot = vm->noFrameRoots;
  while (noFrameRoot != NULL) {
    Value *valuePtr = frameRootValue(noFrameRoot);
    relocate(vm, valuePtr);
    noFrameRoot = frameRootNext(noFrameRoot);
  }

  // relocate noFrameHandlers
  FrameHandler_t noFrameHandler = vm->noFrameHandlers;
  while (noFrameHandler != NULL) {
    Value *valuePtr = frameHandlerValue(noFrameHandler);
    relocate(vm, valuePtr);
    noFrameHandler = frameHandlerNext(noFrameHandler);
  }

  // relocate call stack roots
  Frame_t current = vm->current;
  if (current != NULL) {
    while (true) {

      // relocate fnRef
      {
        Value *ref = getFnRefRef(current);
        relocate(vm, ref);

        Fn *fn = deref(vm, *ref);
        setFn(vm, current, fn);
      }

      uint16_t locals = numLocals(current);
      for (uint16_t i = 0; i < locals; i++) {
        Value *val = getLocalRef(current, i);
        relocate(vm, val);
      }

      uint64_t operands = numOperands(current);
      for (uint64_t i = 0; i < operands; i++) {
        Value *val = getOperandRef(current, i);
        relocate(vm, val);
      }

      FrameRoot_t root = frameRoots(current);
      while (root != NULL) {
        Value *valuePtr = frameRootValue(root);
        relocate(vm, valuePtr);
        root = frameRootNext(root);
      }

      FrameHandler_t handler = frameHandlers(current);
      while (handler != NULL) {
        Value *valuePtr = frameHandlerValue(handler);
        relocate(vm, valuePtr);
        handler = frameHandlerNext(handler);
      }

      if (!hasParent(current)) {
        break;
      } else {
        current = getParent(current);
      }
    }
  }

  void *scanptr = vm->gc.currentHeap;

  uint64_t count = 0;

  // relocate all the objects this object references
  while (scanptr < vm->gc.allocPtr) {
    ObjectHeader *header = scanptr;

    ValueType type = objectHeaderValueType(*header);
    uint64_t sizeBytes = objectHeaderSizeBytes(*header);

    _relocateChildren(vm, type, scanptr);

    scanptr += sizeBytes;
    count++;
  }

  uint64_t newHeapUsed = vm->gc.allocPtr - vm->gc.currentHeap;
  uint64_t sizeRecovered = oldHeapUsed - newHeapUsed;
  uint64_t end = now();
  uint64_t duration = end - start;

  printf("gc: completed, %" PRIu64 " bytes recovered, %" PRIu64 " bytes used, took %" PRIu64 "ms\n", sizeRecovered, newHeapUsed, duration);
}
