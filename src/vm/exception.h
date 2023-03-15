#ifndef WARP_LANG_EXCEPTION_H
#define WARP_LANG_EXCEPTION_H

#include "vm.h"

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

Value exceptionMake(VM_t vm, ExceptionParams p);
Value exceptionMakeRaised(VM_t vm, Raised *raised);
Value exceptionMakeKw(VM_t vm, Raised *raised, wchar_t *kwName);
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

#endif //WARP_LANG_INTERNAL_H

bool hasException(VM_t vm);
void setException(VM_t vm, Value e);
Value getException(VM_t vm);
void clearException(VM_t vm);
