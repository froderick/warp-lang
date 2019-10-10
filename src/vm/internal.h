#include <inttypes.h>
#include "vm.h"

ObjectHeader makeObjectHeader(uint8_t objectType, uint64_t size);
uint8_t objectHeaderType(ObjectHeader h);
uint64_t objectHeaderSize(ObjectHeader h);
uint64_t objectHeaderSizeBytes(ObjectHeader h);
ValueType objectHeaderValueType(ObjectHeader header);
ValueType valueType(Value v);
Value wrapBool(bool b);
bool unwrapBool(Value v);
Value wrapUint(uint64_t i);
uint64_t unwrapUint(Value v);
Value wrapChar(wchar_t v);
wchar_t unwrapChar(Value v);

typedef struct Frame *Frame_t;
typedef struct FrameRoot *FrameRoot_t;
typedef struct FrameHandler *FrameHandler_t;

wchar_t* fnName(Fn *fn);
Value* fnConstants(Fn *fn);
uint8_t* fnCode(Fn *fn);
wchar_t* fnSourceFileName(Fn *fn);
LineNumber* fnLineNumbers(Fn *fn);
Value* closureCaptures(Closure *closure);
wchar_t* stringValue(String *x);

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

/*
 * VM Data Structures
 */

// instruction definitions

typedef int (*Eval) (struct VM *vm, Frame_t frame);

typedef struct Inst {
  const char *name;
  void (*print)(int *i, const char* name, uint8_t *code);
  Eval eval;
} Inst;

typedef struct InstTable {
  uint8_t numInstructions;
  Inst instructions[256];
} InstTable;

// value type definitions

typedef struct Invocable {
  Value ref;
  Fn *fn;
  Closure *closure;
} Invocable;

typedef void (*RelocateChildren)(VM_t vm, void *obj);

typedef struct ValueTypeInfo {
  const char *name;
  bool (*isTruthy)(Value value);
  RelocateChildren relocateChildren;
} ValueTypeInfo;

typedef struct ValueTypeTable {
  uint8_t numValueTypes;
  ValueTypeInfo valueTypes[256];
} ValueTypeTable;

// vm state

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

typedef struct TableEntry {
  bool used;
  Value name;
  uint32_t nameHash;
  Value value;
} TableEntry;

typedef struct Table {
  uint64_t size;
  uint64_t numAllocatedEntries;
  TableEntry *entries;
  // load
} Table;

typedef struct VM {
  VMConfig config;
  GC gc;
  InstTable instTable;
  ValueTypeTable valueTypeTable;
  Stack stack;
  FrameRoot_t noFrameRoots;
  FrameHandler_t noFrameHandlers;
  Frame_t current;
  Table symbolTable;
  Table keywordTable;
  Value exception;
} VM;

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


uint64_t padAllocSize(uint64_t length);

uint32_t stringHash(String *s);

void tableInit(Table *table);
void tableFreeContents(Table *t);
Value tableLookup(VM *vm, Table *table, Value name);
void putEntry(VM *vm, Table *table, Value name, Value insertMe);

// stack

void stackInitContents(Stack *pool, uint64_t segmentSize);

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
void setFnRef(VM *vm, Frame_t frame, Value value);
Value* getFnRefRef(Frame_t frame);

void setFn(VM *vm, Frame_t frame, Fn *fn);

bool hasResult(Frame_t frame);
bool hasParent(Frame_t frame);
Frame_t getParent(Frame_t frame);
void setResult(Frame_t frame, Value result);
Value getResult(Frame_t frame);

typedef struct ExceptionHandler {
  uint16_t jumpAddress;
  uint16_t localIndex;
} ExceptionHandler;

bool hasFnName(Frame_t frame);
wchar_t* getFnName(Frame_t frame);

bool hasSourceTable(Frame_t frame);
bool getLineNumber(Frame_t frame, uint64_t *lineNumber);
wchar_t* getFileName(Frame_t frame);

bool hasException(VM *vm);
void setException(VM *vm, Value e);
Value getException(VM *vm);
void clearException(VM *vm);

void* stackAllocate(Stack *stack, size_t size, char *description);
void stackFree(Stack *stack, void *ptr);

Frame_t pushFrame(VM *vm, Value newFn);
Frame_t replaceFrame(VM *vm, Value newFn);
Frame_t popFrame(VM *vm);

FrameRoot_t frameRoots(Frame_t frame);
Value* frameRootValue(FrameRoot_t root);
FrameRoot_t frameRootNext(FrameRoot_t root);

FrameHandler_t frameHandlers(Frame_t frame);
Value* frameHandlerValue(FrameHandler_t root);
uint16_t frameHandlerJumpAddr(FrameHandler_t handler);
FrameHandler_t frameHandlerNext(FrameHandler_t root);
void pushFrameHandler(VM *vm, Value value, uint16_t jumpAddr);
void popFrameHandler(VM *vm);
void popSpecificFrameHandler(Frame_t frame);
FrameHandler_t currentHandler(Frame_t frame);
void setCurrentHandler(Frame_t frame, FrameHandler_t currentHandler);

Frame* popSpecificFrame(VM *vm, Frame_t popped);
void pushFrameRoot(VM *vm, Value *rootPtr);
void popFrameRoot(VM *vm);

void GCFreeContents(GC *gc);
void GCInitContents(GC *gc);
void GCCreate(GC *gc, uint64_t maxHeapSize);
void collect(VM *vm);
void* alloc(VM *vm, uint64_t length);
void* deref(VM *vm, Value value);
void relocate(VM *vm, Value *valuePtr);
uint64_t now();
void collect(VM *vm);

// values

void fnInitContents(Fn *fn);

void stringInitContents(String *s);
Value makeString(VM *vm, uint64_t length);
Value makeStringValue(VM *vm, wchar_t *text, uint64_t length);

void symbolInitContents(Symbol *s);
Value symbolIntern(VM *vm, Value *protectedName);

void keywordInitContents(Keyword *k);
Value keywordIntern(VM *vm, Value *protectedName);

void consInitContents(Cons *c);
Cons* makeCons(VM *vm);

void arrayInitContents(Array *array);
Value* arrayElements(Array *array);
Array* makeArray(VM *vm, uint64_t size);

void byteArrayInitContents(ByteArray *array);
uint8_t* byteArrayElements(ByteArray *array);
ByteArray* makeByteArray(VM *vm, uint64_t size);

void charArrayInitContents(CharArray *array);
wchar_t* charArrayElements(CharArray *array);
CharArray* makeCharArray(VM *vm, uint64_t size);

#define W_MAP_MIN_ENTRIES 16
#define W_MAP_MIN_LOAD .40
#define W_MAP_MAX_LOAD .70
void _mapEntryInitContents(MapEntry *e);
MapEntry* makeMapEntry(VM *vm);
void _mapInitContents(Map *m);
Map* makeMap(VM *vm);

void recordInitContents(Record *record);
Value* recordFields(Record *record);
Record* makeRecord(VM *vm, uint64_t numFields);

void cFnInitContents(CFn *fn);
wchar_t* cFnName(CFn *fn);
typedef int (*CFnInvoke) (VM_t vm, Frame_t frame);
Value makeCFn(VM *vm, const wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr);
void defineCFn(VM *vm, wchar_t *name, uint16_t numArgs, bool varArgs, CFnInvoke ptr);

#define RAISE_MSG_LENGTH 1023

typedef struct Raised {
  wchar_t message[RAISE_MSG_LENGTH + 1];
  const char *fileName;
  uint64_t lineNumber;
  const char *functionName;
} Raised;

typedef struct ExceptionParams {
  Value *protectedMessage; // includes a string :message
  Value *protectedValue;   // includes a :value
  Raised *raised;          // includes native frame line info
} ExceptionParams;

Value _exceptionMake(VM *vm, ExceptionParams p);
Value exceptionMakeRaised(VM *vm, Raised *raised);
Value exceptionMakeKw(VM *vm, Raised *raised, wchar_t *kwName);
void raisedInitContents(Raised *r);

#define raise(vm, str, ...) {\
  Raised r; \
  raisedInitContents(&r); \
  r.fileName = __FILE__; \
  r.lineNumber = __LINE__; \
  r.functionName = __func__; \
  \
  int len = 64; \
  char msg[len]; \
  snprintf(msg, len, str, ##__VA_ARGS__); \
  \
  swprintf(r.message, ERROR_MSG_LENGTH, L"vm raised an exception: %s", msg); \
  vm->exception = exceptionMakeRaised(vm, &r); \
}

#define raiseKw(vm, kwName) {\
  Raised r; \
  raisedInitContents(&r); \
  r.fileName = __FILE__; \
  r.lineNumber = __LINE__; \
  r.functionName = __func__; \
  \
  vm->exception = exceptionMakeKw(vm, &r, kwName); \
}

void initCFns(VM *vm);

void putMapEntry(VM *vm, Map **protectedMap, Value key, Value insertMe);

int consEval(VM *vm, Frame_t frame);
int addEval(VM *vm, Frame_t frame);
int cmpEval(VM *vm, Frame_t frame);
int subEval(VM *vm, Frame_t frame);

ValueTypeTable valueTypeTableCreate();
const char* getValueTypeName(VM *vm, uint8_t type);
bool isTruthy(VM *vm, Value value);

InstTable instTableCreate();
