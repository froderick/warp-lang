#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include "../errors.h"
#include "internal.h"

/*
 * Call Stack Implementation
 */

void _stackSegmentInitContents(StackSegment *segment) {
  segment->data = NULL;
  segment->dataEnd = NULL;
  segment->allocPtr = NULL;
  segment->prev = NULL;
  segment->next = NULL;
}

void stackInitContents(Stack *pool, uint64_t segmentSize) {
  pool->segmentSize = segmentSize;
  pool->root = NULL;
  pool->current = NULL;
}

StackSegment* makeSegment(uint64_t segmentSize) {

  StackSegment *segment = malloc(sizeof(StackSegment));
  if (segment == NULL) {
    explode("failed to allocate stack segment memory")
  }

  _stackSegmentInitContents(segment);

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

void _freeSegment(StackSegment *segment) {
  if (segment != NULL) {
    free(segment->data);
    _stackSegmentInitContents(segment);
    free(segment);
  }
}

void* _stackAllocate(Stack *stack, size_t size, char *description) {

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
void _stackFree(Stack *stack, void *ptr) {

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
    _freeSegment(freeMe);
  }
  stack->current->next = NULL;

  // update alloc for current segment
  stack->current->allocPtr = ptr;
}

uint64_t _stackSize(Stack *stack) {

  uint64_t result = 0;

  StackSegment *cursor = stack->root;
  while (cursor != NULL) {
    result += stack->segmentSize;
    cursor = cursor->next;
  }

  return result;
}

void _stackClear(Stack *stack) {

  StackSegment *cursor = stack->root;
  while (cursor != NULL) {
    free(cursor->data);
    cursor = cursor->next;
  }

  stack->root = NULL;
  stack->current = NULL;
}

void _stackFreeContents(Stack *stack) {
  if (stack != NULL) {
    _stackClear(stack);
  }
}

// frames

/*
 * The ExecFrame and operations it supports
 */

uint8_t readInstruction(Frame *frame) {

  if (frame->pc >= frame->fn->codeLength) {
    explode("cannot read next instruction, no instructions left");
  }

  uint8_t inst = fnCode(frame->fn)[frame->pc];
  frame->pc += 1;
  return inst;
}

uint16_t readIndex(Frame *frame) {

  if (frame->pc + 1 >= frame->fn->codeLength) {
    explode("cannot read next instruction, no instructions left");
  }

  uint8_t *code = fnCode(frame->fn);
  uint16_t pc = frame->pc;
  uint16_t index = (code[pc] << 8) | code[pc + 1];
  frame->pc += 2;

  return index;
}

void setPc(Frame *frame, uint16_t newPc) {
  if (newPc >= frame->fn->codeLength) {
    explode("no such instruction: %u", newPc);
  }
  frame->pc = newPc;
}

Value getConst(Frame *frame, uint16_t constantIndex) {
  if (constantIndex >= frame->fn->numConstants) {
    explode("no such constant: %u", constantIndex);
  }
  return fnConstants(frame->fn)[constantIndex];
}

Value getLocal(Frame *frame, uint16_t localIndex) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  return frame->locals[localIndex];
}

void setLocal(Frame *frame, uint16_t localIndex, Value value) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  if (value == 0) {
    explode("invalid local value (NULL): %u", localIndex);
  }
  frame->locals[localIndex] = value;
}

Value* getLocalRef(Frame *frame, uint16_t localIndex) {
  if (localIndex >= frame->fn->numLocals) {
    explode("no such local: %u", localIndex);
  }
  return &frame->locals[localIndex];
}

uint16_t numLocals(Frame *frame) {
  return frame->fn->numLocals;
}

uint64_t numOperands(Frame *frame) {
  return frame->opStackUsedDepth;
}

Value* getOperandRef(Frame *frame, uint64_t opIndex) {
  if (opIndex >= frame->opStackUsedDepth) {
    explode("no such operand: %" PRIu64, opIndex);
  }
  return &frame->opStack[opIndex];
}

void pushOperand(Frame *frame, Value value) {
  if (frame->opStackMaxDepth == frame->opStackUsedDepth) {
    explode("cannot allocate op stack greater than max %" PRIu64, frame->opStackMaxDepth);
  }
  frame->opStack[frame->opStackUsedDepth] = value;
  frame->opStackUsedDepth++;
}

Value popOperand(Frame *frame) {
  if (frame->opStackUsedDepth == 0) {
    explode("cannot pop from empty op stack")
  }
  frame->opStackUsedDepth--;
  return frame->opStack[frame->opStackUsedDepth];
}

Value getFnRef(Frame *frame) {
  return frame->fnRef;
}

void setFnRef(VM *vm, Frame *frame, Value value) {
  frame->fnRef = value;
  frame->fn = deref(vm, value);
}

Value* getFnRefRef(Frame_t frame) {
  return &(frame->fnRef);
}

void setFn(VM *vm, Frame *frame, Fn *fn) {
  frame->fn = fn;
}

bool hasResult(Frame *frame) {
  return frame->resultAvailable;
}

bool hasParent(Frame *frame) {
  return frame->parent != NULL;
}

Frame_t getParent(Frame *frame) {
  if (frame->parent == NULL) {
    explode("no parent available");
  }
  return frame->parent;
}

void setResult(Frame *frame, Value result) {
  if (frame->resultAvailable) {
    explode("result already set");
  }
  frame->result = result;
  frame->resultAvailable = true;
}

Value getResult(Frame *frame) {
  if (!frame->resultAvailable) {
    explode("result not set");
  }
  return frame->result;
}

bool hasFnName(Frame *frame) {
  return frame->fn->hasName;
}

wchar_t* getFnName(Frame_t frame) {
  if (!frame->fn->hasName) {
    explode("no fn name found");
  }
  return fnName(frame->fn);
}

bool hasSourceTable(Frame *frame) {
  return frame->fn->hasSourceTable;
}

bool getLineNumber(Frame *frame, uint64_t *lineNumber) {
  if (frame->fn->hasSourceTable) {
    for (uint64_t i=0; i<frame->fn->numLineNumbers; i++) {
      LineNumber *l = &fnLineNumbers(frame->fn)[i];
      if (l->startInstructionIndex >= frame->pc) {
        break;
      }
      else {
        *lineNumber = l->lineNumber;
      }
    }
  }
  return false;
}

wchar_t* getFileName(Frame_t frame) {
  if (frame->fn->hasSourceTable) {
    return fnSourceFileName(frame->fn);
  }
  return NULL;
}

void _frameInitContents(Frame *frame) {
  frame->parent = NULL;
  frame->fnRef = W_NIL_VALUE;
  frame->fn = NULL;
  frame->locals = NULL;
  frame->opStack = NULL;
  frame->resultAvailable = 0;
  frame->result = W_NIL_VALUE;
  frame->pc = 0;

  frame->opStackUsedDepth = 0;
  frame->opStackMaxDepth = 0;
  frame->opStack = NULL;

  frame->roots = NULL;
  frame->handlers = NULL;

  frame->currentHandler = NULL;
}

Frame_t pushFrame(VM *vm, Value newFn) {

  Fn *fn = deref(vm, newFn);

  Stack *stack = &vm->stack;
  Frame *parent = vm->current;

  Frame *frame = _stackAllocate(stack, sizeof(Frame), "ExecFrame");
  _frameInitContents(frame);

  frame->parent = parent;
  frame->fnRef = newFn;
  frame->fn = fn;

  frame->locals = _stackAllocate(stack, sizeof(Value) * frame->fn->numLocals, "locals");
  for (uint16_t i = 0; i < frame->fn->numLocals; i++) {
    frame->locals[i] = W_NIL_VALUE;
  }

  frame->opStackMaxDepth = frame->fn->maxOperandStackSize;
  frame->opStackUsedDepth = 0;
  frame->opStack = _stackAllocate(stack, sizeof(Value) * frame->opStackMaxDepth, "opStack");

  vm->current = frame;


  return frame;
}

Frame* replaceFrame(VM *vm, Value newFn) {
  Fn *fn = deref(vm, newFn);

  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  if (fn->numLocals > frame->fn->numLocals) {
    frame->locals = _stackAllocate(stack, sizeof(Value) * fn->numLocals, "locals");
  }
  for (uint16_t i = 0; i < fn->numLocals; i++) {
    frame->locals[i] = W_NIL_VALUE;
  }

  if (fn->maxOperandStackSize > frame->opStackMaxDepth) {

    uint16_t oldSize = frame->opStackMaxDepth;
    uint16_t newSize = fn->maxOperandStackSize;

    Value* newOpStack = _stackAllocate(stack, sizeof(Value) * newSize, "opStack");
    memcpy(newOpStack, frame->opStack, oldSize * sizeof(Value));

    frame->opStackMaxDepth = newSize;
    frame->opStack = newOpStack;
  }

  frame->fnRef = newFn;
  frame->fn = fn;
  frame->result = W_NIL_VALUE;
  frame->resultAvailable = false;
  frame->pc = 0;

  return frame;
}

Frame* popFrame(VM *vm) {
  if (vm->current == NULL) {
    explode("no frames on stack");
  }
  Frame *popped = vm->current;
  vm->current = vm->current->parent;
  _stackFree(&vm->stack, popped);
  return vm->current;
}

FrameRoot_t frameRoots(Frame_t frame) {
  return frame->roots;
}

Value* frameRootValue(FrameRoot_t root) {
  return root->valuePtr;
}

FrameRoot_t frameRootNext(FrameRoot_t root) {
  return root->next;
}

FrameHandler_t frameHandlers(Frame_t frame) {
  return frame->handlers;
}

Value* frameHandlerValue(FrameHandler_t handler) {
  return &handler->value;
}

uint16_t frameHandlerJumpAddr(FrameHandler_t handler) {
  return handler->jumpAddr;
}

FrameHandler_t frameHandlerNext(FrameHandler_t handler) {
  return handler->next;
}

void _frameHandlerInitContents(FrameHandler *handler) {
  handler->value = W_NIL_VALUE;
  handler->jumpAddr = 0;
  handler->next = NULL;
}

void pushFrameHandler(VM *vm, Value value, uint16_t jumpAddr) {
  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  FrameHandler *handler = _stackAllocate(stack, sizeof(FrameHandler), "FrameHandler");
  _frameHandlerInitContents(handler);
  handler->value = value;
  handler->jumpAddr = jumpAddr;

  if (frame == NULL) {
    handler->next = vm->noFrameHandlers;
    vm->noFrameHandlers = handler;
  }
  else {
    handler->next = frame->handlers;
    frame->handlers = handler;
  }
}

void popFrameHandler(VM *vm) {
  Frame *frame = vm->current;
  if (frame == NULL) {
    vm->noFrameHandlers = vm->noFrameHandlers->next;
  }
  else {
    frame->handlers = frame->handlers->next;
  }
}

void popSpecificFrameHandler(Frame_t frame) {
  frame->handlers = frame->handlers->next;
}

FrameHandler_t currentHandler(Frame_t frame) {
  return frame->currentHandler;
}

void setCurrentHandler(Frame_t frame, FrameHandler_t currentHandler) {
  frame->currentHandler = currentHandler;
}

Frame* popSpecificFrame(VM *vm, Frame_t popped) {
  if (vm->current == NULL) {
    explode("no frames on stack");
  }
  vm->current = vm->current->parent;
  _stackFree(&vm->stack, popped);
  return vm->current;
}

void pushFrameRoot(VM *vm, Value *rootPtr) {
  Stack *stack = &vm->stack;
  Frame *frame = vm->current;

  if (frame == NULL) {
    FrameRoot *root = _stackAllocate(stack, sizeof(FrameRoot), "FrameRoot");
    root->valuePtr = rootPtr;
    root->next = vm->noFrameRoots;
    vm->noFrameRoots = root;
  }
  else {
    FrameRoot *root = _stackAllocate(stack, sizeof(FrameRoot), "FrameRoot");
    root->valuePtr = rootPtr;
    root->next = frame->roots;
    frame->roots = root;
  }
}

void popFrameRoot(VM *vm) {
  Frame *frame = vm->current;
  if (frame == NULL) {
    vm->noFrameRoots = vm->noFrameRoots->next;
  }
  else {
    frame->roots = frame->roots->next;
  }
}
