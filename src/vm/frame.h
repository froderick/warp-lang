#ifndef WARP_LANG_FRAME_H
#define WARP_LANG_FRAME_H

#include "vm.h"

// stack

typedef struct StackSegment StackSegment;

typedef struct StackSegment {
  void *data;         // the first valid address in the segment
  void *dataEnd;      // the first address after the end of the segment
  void *allocPtr;     // the offset to use for allocation
  StackSegment *prev; // the previous segment, may be NULL
  StackSegment *next; // the subsequent segment, may be NULL
} StackSegment;

typedef struct Stack {
  uint64_t segmentSize;  // the size of each allocated segment (configuration)
  StackSegment *root;    // the first segment to be allocated
  StackSegment *current; // the most recent segment to be allocated
} Stack;

void stackInitContents(Stack *pool, uint64_t segmentSize);
void* stackAllocate(Stack *stack, size_t size, char *description);
void stackFree(Stack *stack, void *ptr);

// stack frame

typedef struct Frame *Frame_t;
typedef struct FrameRoot *FrameRoot_t;
typedef struct FrameHandler *FrameHandler_t;

typedef struct Frame Frame;

typedef struct FrameRoot FrameRoot;

typedef struct FrameRoot {
  Value* valuePtr;
  FrameRoot *next;
} FrameRoot;

typedef struct FrameHandler FrameHandler;

typedef struct FrameHandler {
  Value value;
  uint16_t jumpAddr;
  FrameHandler *next;
} FrameHandler;

typedef struct Frame {
  Frame *parent;

  Value fnRef;
  Fn *fn;

  Value *locals;

  uint64_t opStackMaxDepth;
  uint64_t opStackUsedDepth;
  Value *opStack;

  FrameRoot *roots;
  FrameHandler *handlers;

  Value result;
  bool resultAvailable;
  uint16_t pc;

  /*
   * Only non-null if a frame is executing a handler. References a handler in the parent frame.
   *
   * This reference is used o handle the case where an exception is thrown through the top
   * of a handler-function. This allows handleRaise() to pick the next highest handler within
   * the parent function to handle this problem, if it exists. Otherwise handleRaise() crawls
   * up the stack as normal.
   */
  FrameHandler_t currentHandler;
} Frame;

// frames

/*
 * The ExecFrame and operations it supports
 */

uint8_t readInstruction(Frame_t frame);
uint16_t readIndex(Frame_t frame);
void setPc(Frame_t frame, uint16_t newPc);
Value getConst(Frame_t frame, uint16_t constantIndex);
Value getLocal(Frame_t frame, uint16_t localIndex);
void setLocal(Frame_t frame, uint16_t localIndex, Value value);
Value* getLocalRef(Frame_t frame, uint16_t localIndex);
uint16_t numLocals(Frame_t frame);
uint64_t numOperands(Frame_t frame);
Value* getOperandRef(Frame_t frame, uint64_t opIndex);
void pushOperand(Frame_t frame, Value value);
Value popOperand(Frame_t frame);
Value getFnRef(Frame_t frame);
void setFnRef(VM_t vm, Frame_t frame, Value value);
Value* getFnRefRef(Frame_t frame);

void setFn(VM_t vm, Frame_t frame, Fn *fn);

bool hasResult(Frame_t frame);
bool hasParent(Frame_t frame);
Frame_t getParent(Frame_t frame);
void setResult(Frame_t frame, Value result);
Value getResult(Frame_t frame);

bool hasFnName(Frame_t frame);
wchar_t* getFnName(Frame_t frame);

bool hasSourceTable(Frame_t frame);
bool getLineNumber(Frame_t frame, uint64_t *lineNumber);
wchar_t* getFileName(Frame_t frame);

Frame_t pushFrame(VM_t vm, Value newFn);
Frame_t replaceFrame(VM_t vm, Value newFn);
Frame_t popFrame(VM_t vm);

FrameRoot_t frameRoots(Frame_t frame);
Value* frameRootValue(FrameRoot_t root);
FrameRoot_t frameRootNext(FrameRoot_t root);

FrameHandler_t frameHandlers(Frame_t frame);
Value* frameHandlerValue(FrameHandler_t root);
uint16_t frameHandlerJumpAddr(FrameHandler_t handler);
FrameHandler_t frameHandlerNext(FrameHandler_t root);
void pushFrameHandler(VM_t vm, Value value, uint16_t jumpAddr);
void popFrameHandler(VM_t vm);
void popSpecificFrameHandler(Frame_t frame);
FrameHandler_t currentHandler(Frame_t frame);
void setCurrentHandler(Frame_t frame, FrameHandler_t currentHandler);

Frame* popSpecificFrame(VM_t vm, Frame_t popped);
void pushFrameRoot(VM_t vm, Value *rootPtr);
void popFrameRoot(VM_t vm);


#endif //WARP_LANG_FRAME_H
