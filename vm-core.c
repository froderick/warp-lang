#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <inttypes.h>
#include "errors.h"

/*
 * Core data structures
 */

typedef enum ValueType {

  // non-heap objects
  VT_NIL,
  VT_BOOL,
  VT_UINT,
  VT_CHAR,

  // heap objects
  VT_ARRAY,
  VT_RECORD,
  VT_FN,

} ValueType;

typedef struct Value {
  ValueType type : 4;
  uint64_t value : 60;
} Value;

/*
 * Heap data structures
 */

//  This struct must be first field inside all heap objects. It must come first so that the GC can
//  scan through the heap, for which it needs to determine object sizes and object types.
typedef struct ObjectHeader {
  ValueType type : 4;
  uint64_t size : 60;
  Value metadata;
} ObjectHeader;

typedef struct Array {
  ObjectHeader header;
  uint8_t elementWidth;
  uint64_t length;
} Array;

typedef struct Record {
  ObjectHeader header;
} Record;

typedef void* Fn_t;

typedef struct Fn {
  ObjectHeader header;
  Fn_t fnPtr;
} Fn;

typedef struct Heap {

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

} Heap;

/*
 * Stack data structures
 */

typedef struct StackSegment StackSegment;

typedef struct StackSegment {
  void *data;           // the first valid address in the segment
  void *dataEnd;        // the first address after the end of the segment
  void *allocPtr;       // the offset to use for allocation
  StackSegment *prev;        // the previous segment, may be NULL
  StackSegment *next;        // the subsequent segment, may be NULL
} StackSegment;

typedef struct Stack {
  uint64_t segmentSize; // the size of each allocated segment (configuration)
  StackSegment *root;        // the first segment to be allocated
  StackSegment *current;     // the most recent segment to be allocated
} Stack;

typedef struct Frame Frame;

typedef struct Frame {

  Frame *parent;

  /*
   * This is a place for the interpreter to shove frame-specific information
   * that the core does not care about, such as pc, exception info, etc.
   */
  void *activationRecord;
  uint64_t activationRecordSize;

  Value *locals;

  uint64_t opStackMaxDepth;
  uint64_t opStackUsedDepth;
  Value *opStack;

  Value result;

} Frame;

/*
 * RefRegistry data structures
 */

typedef struct RefElem RefElem;

typedef struct RefElem {
  Value heapObject;
  RefElem *prev, *next;
} RefElem;

typedef struct RefRegistry {
  RefElem *used;
  RefElem *free;
} RefRegistry;

/*
 * A Ref is really a direct memory pointer to a RefElem struct.
 */
typedef uint64_t Ref;

/*
 * ValueType protocol data structures
 */

typedef struct VMCore VMCore;

typedef void (*RelocateChildren)(VMCore *core, void *oldHeap, void *obj);

typedef struct ValueTypeInfo {
  const char *name;
  bool isHeapObject;
  RelocateChildren relocateChildren;
} ValueTypeInfo;

typedef struct ValueTypeTable {
  uint8_t numValueTypes;
  ValueTypeInfo valueTypes[256];
} ValueTypeTable;

typedef struct VMCore {
  Heap heap;
  Stack stack;
  RefRegistry refs;
  ValueTypeTable valueTypeTable;
} VMCore;

/*
 * Non-Heap Value initializers
 */

Value nil() {
  Value v;
  v.type = VT_NIL;
  v.value = 0;
  return v;
}

/*
 * Heap data structure initializers, accessors
 */

void objectHeaderInitContents(ObjectHeader *h) {
  h->type = VT_NIL;
  h->size = 0;
  h->metadata = nil();
}

void arrayInitContents(Array *a) {
  objectHeaderInitContents(&a->header);
  a->elementWidth = 0;
  a->length = 0;
}

Value* arrayElements(Array *x) { return (void*)x + sizeof(Array); }

void recordInitContents(Record *r) {
  objectHeaderInitContents(&r->header);
}

Value* recordFields(Record *x) { return (void*)x + sizeof(Record); }

void fnInitContents(Fn *fn) {
  objectHeaderInitContents(&fn->header);
  fn->fnPtr = NULL;
}

/*
 * ValueType protocols
 */

bool isHeapObject(VMCore *core, Value value) {
  return core->valueTypeTable.valueTypes[value.type].isHeapObject;
}

void relocateChildren(VMCore *core, ValueType type, void *oldHeap, void *obj) {
  RelocateChildren relocate = core->valueTypeTable.valueTypes[type].relocateChildren;
  if (relocate != NULL) {
    relocate(core, oldHeap, obj);
  }
}

/*
 * Heap implementation
 * super useful: http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm
 */

void heapFreeContents(Heap *heap) {
  free(heap->heapMemory);
  heap->heapMemory = NULL;
  heap->heapA = NULL;
  heap->heapB = NULL;
  heap->currentHeap = NULL;
  heap->allocPtr = NULL;
}

void heapInitContents(Heap *heap) {
  heap->heapMemorySize = 0;
  heap->heapMemory = NULL;
  heap->heapSize = 0;
  heap->heapA = NULL;
  heap->heapB = NULL;
  heap->currentHeap = NULL;
  heap->allocPtr = NULL;
}

void heapCreate(Heap *heap, uint64_t maxHeapSize) {

  heapInitContents(heap);

  heap->heapSize = maxHeapSize;
  heap->heapMemorySize = heap->heapSize * 2;

  heap->heapMemory = malloc(heap->heapMemorySize);
  if (heap->heapMemory == NULL) {
    explode("failed to allocate heap memory");
  }
  memset(heap->heapMemory, 0, heap->heapMemorySize);

  heap->heapA = heap->heapMemory;
  heap->heapB = heap->heapA + heap->heapSize;
  heap->currentHeap = heap->heapA;
  heap->currentHeapEnd = heap->currentHeap + heap->heapSize;
  heap->allocPtr = heap->currentHeap;
}

void collect(VMCore *core);

#define R_OOM 1

/*
 * Allocates, returns R_OOM if allocation fails. Doesn't attempt collection.
 */
int _alloc(Heap *heap, uint64_t length, void **ptr, uint64_t *offset) {

  /*
   * each object must be at least the size of a pointer, so we can replace it with a
   * forwarding pointer during collection
   */
  if (length < 8) {
    length = 8;
  }

  if (heap->allocPtr + length < heap->currentHeapEnd) {
    *ptr = heap->allocPtr;
    *offset = heap->allocPtr - heap->currentHeap;
    heap->allocPtr += length;
    return R_SUCCESS;
  }
  else {
    return R_OOM;
  }
}

/*
 * Allocates, attempts collection if allocation fails.
 */
void* alloc(VMCore *core, uint64_t length, Value *value) {

  void *ptr = NULL;
  uint64_t offset = 0;

  int success = _alloc(&core->heap, length, &ptr, &offset);

  if (success == R_OOM) {
    collect(core);

    success = _alloc(&core->heap, length, &ptr, &offset);

    if (success == R_OOM) {
      explode("out of memory, failed to allocate %" PRIu64 " bytes", length);
    }
  }

  value->value = offset;

  return ptr;
}

void* deref(Heap *heap, uint64_t offset) {
  if (offset > heap->heapSize) {
    explode("invalid memory address");
  }
  return heap->currentHeap + offset;
}

bool inCurrentHeap(Heap *heap, void *ptr) {
  return heap->currentHeap <= ptr && ptr < heap->currentHeapEnd;
}

void relocate(VMCore *core, void *oldHeap, Value *value) {

  if (!isHeapObject(core, *value)) {
    // only relocate heap objects
    return;
  }

  Heap *heap = &core->heap;

  void *ptr = NULL;
  if (value->value > heap->heapSize) {
    explode("invalid memory address");
  }
  ptr = oldHeap + value->value;

  void **forwardPtr = ptr;
  if (inCurrentHeap(heap, *forwardPtr)) {
    value->value = *forwardPtr - heap->currentHeap;
  }
  else {
    size_t size = ((ObjectHeader*)ptr)->size;

    void *newPtr = NULL;
    uint64_t offset = 0;

    if (_alloc(heap, size, &newPtr, &offset) == R_OOM) {
      explode("out of memory, cannot allocate %lu bytes mid-collection", size);
    }

    memcpy(newPtr, ptr, size);
    value->value = newPtr - heap->currentHeap;

    *forwardPtr = newPtr;
  }
}

uint64_t now() {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC_RAW, &now);
  uint64_t millis = now.tv_nsec / 1000000;
  return millis;
}

void collect(VMCore *core) {

  uint64_t oldHeapUsed = core->heap.allocPtr - core->heap.currentHeap;

  uint64_t start = now();
  printf("collection: starting, %" PRIu64 " bytes used\n", oldHeapUsed);

  // flip heaps
  void *oldHeap = core->heap.currentHeap;
  if (oldHeap == core->heap.heapA) {
    core->heap.currentHeap = core->heap.heapB;
  }
  else {
    core->heap.currentHeap = core->heap.heapA;
  }
  core->heap.currentHeapEnd = core->heap.currentHeap + core->heap.heapSize;
  core->heap.allocPtr = core->heap.currentHeap;

  memset(core->heap.currentHeap, 0, core->heap.heapSize);

  // TODO: visit held references

  // TODO: rewrite the stack and traverse it here
  // relocate call stack roots
  /*ExecFrame_t current = frame;
  while (true) {

    // relocate fnRef
    {
      Value oldFnRef = getFnRef(current);
      Value newFnRef = oldFnRef;
      throws(relocate(vm, oldHeap, &newFnRef, error));

      if (oldFnRef.value != newFnRef.value) {
        throws(setFnRef(vm, current, newFnRef, error));
      }
    }

    uint16_t locals = numLocals(current);
    for (uint16_t i=0; i<locals; i++) {

      Value *val = NULL;
      throws(getLocalRef(current, i, &val, error));

      throws(relocate(vm, oldHeap, val, error));
    }

    uint64_t operands = numOperands(current);
    for (uint64_t i=0; i<operands; i++) {

      Value *val = NULL;
      throws(getOperandRef(current, i, &val, error));

      throws(relocate(vm, oldHeap, val, error));
    }

    if (!hasParent(current)) {
      break;
    }
    else {
      throws(getParent(current, &current, error) != R_SUCCESS);
    }
  }*/

  void *scanptr = core->heap.currentHeap;

  // relocate all the objects this object references
  while (scanptr < core->heap.allocPtr) {
    ObjectHeader *header = scanptr;
    relocateChildren(core, header->type, oldHeap, scanptr);
    scanptr += header->size;
  }

  uint64_t newHeapUsed = core->heap.allocPtr - core->heap.currentHeap;
  uint64_t sizeRecovered = oldHeapUsed - newHeapUsed;
  uint64_t end = now();
  uint64_t duration = end - start;

  printf("collection: completed, %" PRIu64 " bytes recovered, %" PRIu64 " bytes used, took %" PRIu64 "ms\n", sizeRecovered, newHeapUsed, duration);
}

/*
 * ValueType protocol implementations
 */

void relocateChildrenArray(VMCore *core, void *oldHeap, void *obj) {
  Array *arr = obj;
  for (uint16_t i=0; i<arr->length; i++) {
    relocate(core, oldHeap, &arrayElements(arr)[i]);
  }
}

void relocateChildrenRecord(VMCore *core, void *oldHeap, void *obj) {
  Record *record = obj;

  void *ptr = recordFields(record);
  void *recordFieldsEnd = obj + record->header.size;

  while (ptr < recordFieldsEnd) {
    relocate(core, oldHeap, (Value*)ptr);
  }
}

ValueTypeTable valueTypeTableCreate() {
  ValueTypeTable table;

  // init table with blanks
  uint16_t valueTypesAllocated = sizeof(table.valueTypes) / sizeof(table.valueTypes[0]);
  for (int i=0; i<valueTypesAllocated; i++) {
    table.valueTypes[i].name = NULL;
    table.valueTypes[i].isHeapObject = false;
    table.valueTypes[i].relocateChildren = NULL;
  }

  // init with known value types
  ValueTypeInfo valueTypes [] = {
      [VT_NIL]       = {.name = "nil",
          .isHeapObject = false,
          .relocateChildren = NULL},
      [VT_UINT]      = {.name = "uint",
          .isHeapObject = false,
          .relocateChildren = NULL},
      [VT_BOOL]      = {.name = "bool",
          .isHeapObject = false,
          .relocateChildren = NULL},
      [VT_CHAR]       = {.name = "char",
          .isHeapObject = true,
          .relocateChildren = NULL},
      [VT_ARRAY]     = {.name = "array",
          .isHeapObject = true,
          .relocateChildren = &relocateChildrenArray},
      [VT_RECORD] = {.name = "record",
          .isHeapObject = true,
          .relocateChildren = &relocateChildrenRecord},
      [VT_FN] = {.name = "fn",
          .isHeapObject = true,
          .relocateChildren = NULL},
  };
  memcpy(table.valueTypes, valueTypes, sizeof(valueTypes));
  table.numValueTypes = sizeof(valueTypes) / sizeof(valueTypes[0]);

  return table;
}

/*
 * Stack implementation
 */

void segmentInitContents(StackSegment *segment) {
  segment->data = NULL;
  segment->dataEnd = NULL;
  segment->allocPtr = NULL;
  segment->prev = NULL;
  segment->next = NULL;
}

void stackInitContents(Stack *pool) {
  pool->segmentSize = 0;
  pool->root = NULL;
  pool->current = NULL;
}

StackSegment* makeSegment(uint64_t segmentSize) {

  StackSegment *segment = malloc(sizeof(StackSegment));
  if (segment == NULL) {
    explode("failed to allocate stack segment memory")
  }

  segmentInitContents(segment);

  segment->data = malloc(segmentSize);
  if (segment->data == NULL) {
    explode("failed to allocate stack segment memory block")
  }

  segment->allocPtr = segment->data;
  segment->dataEnd = segment->data + segmentSize;
  segment->prev = NULL;
  segment->next = NULL;

  return segment;
}

void freeSegment(StackSegment *segment) {
  if (segment != NULL) {
    free(segment->data);
    segmentInitContents(segment);
    free(segment);
  }
}

void* stackAllocate(Stack *stack, size_t size, char *description) {

  if (size == 0) {
    return NULL;
  }

  if (size > stack->segmentSize) {
    explode("cannot allocate a size bigger than %zu: %s", size, description);
  }

  if (stack->root == NULL) { // create root segment if missing
    StackSegment *segment = makeSegment(stack->segmentSize);
    stack->root = segment;
    stack->current = segment;
  }
  else if (stack->current->allocPtr + size >= stack->current->dataEnd) { // add new segment
    StackSegment *segment = makeSegment(stack->segmentSize);
    segment->prev = stack->current;
    stack->current->next = segment;
    stack->current = segment;
  }

  void* ret = stack->current->allocPtr;
  stack->current->allocPtr += size;

  return ret;
}

/*
 * unwinds the stack allocation to immediately before the specified address
 */
void stackFree(Stack *stack, void *ptr) {

  if (stack->root == NULL) {
    explode("stack is not initialized");
  }

  { // bounds check
    void *start = stack->root->data;
    void *end = stack->current->dataEnd;
    if (ptr < start || ptr >= end) {
      explode("cannot free data, it is outside the stack address range [%" PRIu64 " - %" PRIu64 "]: %" PRIu64,
              (uint64_t) start, (uint64_t) end, (uint64_t) ptr);
    }
  }

  // free intermediate segments
  while (ptr < stack->current->data) {
    StackSegment *freeMe = stack->current;
    stack->current = stack->current->prev;
    freeSegment(freeMe);
  }
  stack->current->next = NULL;

  // update alloc for current segment
  stack->current->allocPtr = ptr;
}

uint64_t stackSize(Stack *stack) {

  uint64_t result = 0;

  StackSegment *cursor = stack->root;
  while (cursor != NULL) {
    result += stack->segmentSize;
    cursor = cursor->next;
  }

  return result;
}

void stackClear(Stack *stack) {

  StackSegment *cursor = stack->root;
  while (cursor != NULL) {
    free(cursor->data);
    cursor = cursor->next;
  }

  stack->root = NULL;
  stack->current = NULL;
}

void stackFreeContents(Stack *stack) {
  if (stack != NULL) {
    stackClear(stack);
  }
}

/*
 * Frame implementation
 */

void frameInitContents(Frame *f) {
  f->parent = NULL;
  f->activationRecord = NULL;
  f->activationRecordSize = 0;
  f->locals = NULL;
  f->opStackMaxDepth = 0;
  f->opStackUsedDepth = 0;
  f->opStack = NULL;
  f->result = nil();
}

Frame* pushFrame(Stack *stack,
                 Frame *parent, uint16_t numLocals, uint16_t maxOpStackDepth,
                 void* activationRecordPtr, size_t activationRecordSize) {

  Frame *frame = stackAllocate(stack, sizeof(Frame), "Frame");
  frameInitContents(frame);

  frame->parent = parent;

  frame->activationRecordSize = activationRecordSize;
  frame->activationRecord = stackAllocate(stack, activationRecordSize, "activationRecord");
  memcpy(frame->activationRecord, activationRecordPtr, activationRecordSize);

  frame->locals = stackAllocate(stack, sizeof(Value) * numLocals, "locals");

  frame->opStackMaxDepth = maxOpStackDepth;
  frame->opStack = stackAllocate(stack, sizeof(Value) * maxOpStackDepth, "opStack");

  return frame;
}

Frame* popFrame(Stack *stack, Frame *current) {
  Frame *parent = current->parent;
  stackFree(stack, current);
  return parent;
}

/* RefRegistry implementation
 *
 * The purpose of a registry is to hold references to heap objects from c
 * code in such a way that the collector can discover them and avoid premature
 * collection. Holding these references directly in c code is a problem
 * because the copying gc needs to be able to rewrite all the references
 * periodically. The c call stack is unavailable to the collector, so a layer
 * of indirection is needed to accomplish this.
 *
 * The VMCore has a single global registry for now, which is based on a
 * doubly-linked list. This is not good for cache locality, but we can fix this
 * later. Generally we only need refs when writing c code that manipulates
 * refs outside of places managed exclusively by the VMCore (stack, heap,
 * op-stack, etc).
 */

void refElemInitContents(RefElem *e) {
  e->heapObject = nil();
  e->prev = NULL;
  e->next = NULL;
}

void refRegistryInitContents(RefRegistry *r) {
  r->used = NULL;
  r->free = NULL;
}

void refRegistryFreeContents(RefRegistry *r) {
  if (r != NULL) {
    RefElem *elem = NULL;

    elem = r->free;
    while (elem != NULL) {
      RefElem *freeMe = elem;
      elem = elem->next;
      free(freeMe);
    }
    r->free = NULL;

    elem = r->used;
    while (elem != NULL) {
      RefElem *freeMe = elem;
      elem = elem->next;
      free(freeMe);
    }
    r->used = NULL;
  }
}

Ref createRef(RefRegistry *registry, Value value) {

  RefElem *newHead = NULL;
  if (registry->free != NULL) { // look for free first
    newHead = registry->free;
    registry->free = registry->free->next;
  }
  else { // allocate
    newHead = malloc(sizeof(RefElem));
    if (newHead == NULL) {
      explode("failed to allocate RefElem");
    }
  }

  newHead->heapObject = value;
  newHead->prev = NULL;
  newHead->next = registry->used;

  registry->used = newHead;

  Ref ref = (Ref)&newHead;
  return ref;
}

void destroyRef(RefRegistry *registry, Ref ref) {

  RefElem *elem = (RefElem*)ref;

  // clear
  elem->heapObject = nil();

  // remove
  if (elem->prev != NULL) {
    elem->prev->next = elem->next;
  }
  if (elem->next != NULL) {
    elem->next->prev = elem->prev;
  }

  // add to free list
  elem->prev = NULL;
  elem->next = registry->free;
  registry->free = elem;
}

#define TEN_MB (1024 * 1000 * 10)

void coreInitContents(VMCore *core) {
  core->valueTypeTable = valueTypeTableCreate();
  heapCreate(&core->heap, TEN_MB);
  stackInitContents(&core->stack);
  refRegistryInitContents(&core->refs);
}

void coreFreeContents(VMCore *core) {
  if (core != NULL) {
    heapFreeContents(&core->heap);
    stackFreeContents(&core->stack);
    refRegistryFreeContents(&core->refs);
  }
}

/*
 * VM Core API
 */

typedef struct VMCore *VMCore_t;

VMCore_t coreCreate() {
  VMCore *core = malloc(sizeof(VMCore));
  if (core == NULL) {
    explode("failed to allocate VMCore");
  }
  coreInitContents(core);
  return core;
}

void coreDestroy(VMCore_t core) {
  coreFreeContents(core);
  free(core);
}

Ref coreArrayCreate(VMCore_t core, uint64_t length) {
  Array *arr = NULL;

  uint8_t elementWidth = sizeof(Value);
  uint64_t elementsSize = length * elementWidth;
  size_t size = sizeof(Array) + elementsSize;

  Value value;
  value.type = VT_ARRAY;

  arr = alloc(core, size, &value);

  arrayInitContents(arr);
  arr->header.type = value.type;
  arr->header.size = size;
  arr->length = length;
  arr->elementWidth = elementWidth;

  Value *elements = arrayElements(arr);
  Value init = nil();
  for (uint64_t i=0; i<length; i++) {
    elements[i] = init;
  }

  return createRef(&core->refs, value);
}

void coreRefDestroy(VMCore_t core, Ref ref) {
  destroyRef(&core->refs, ref);
}

// TODO: I had a thought: don't bother making loaded code collectable, there's not much point
// this simplifies lots of things, we make a dedicated memory pool for loaded functions. function
// references should be their own types so we can easily provide a native pointer back to the
// real function definition. someday if collecting unused code becomes important we can worry
// about it then

// non-ref interactions
// - push a local to the op-stack
// - pop from the op-stack to a local
// - push a constant to the op-stack

// - push a local array element to the op-stack by index
// - pop from the op-stack to a local array element by index
// - push a constant array element to the op-stack by index

// - push a local record field to the op-stack by index
// - pop from the op-stack to a local record field by index
// - push a constant record field to the op-stack by index

// interact with heap via refs
// - create array with given width and length
// - get array length
// - get array element width
// - get array element value by index
// - set array element value by index
// - create record with given number of fields
// - get record field value by index
// - set record field value by index
// - create fn with supplied code ptr
// - get fn code ptr

// interact with stack objects via refs
// - get a handle to the current frame
// - get a handle to the parent of a frame
// - push/pop from op-stack by frame
// - get local value by index by frame
// - set local value by index by frame
// - push a new frame on top the current one
// - pop the current frame
// - get a pointer to the activation record
// - get the activation record size
// - get the result
// - set the result

// general ref functions
// - get object type / assert object type
// - delete ref






















































