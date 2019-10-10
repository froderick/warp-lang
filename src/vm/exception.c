#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include "../errors.h"
#include "internal.h"

bool hasException(VM *vm) {
  return vm->exception != W_NIL_VALUE;
}

void setException(VM *vm, Value value) {
  vm->exception = value;
}

Value getException(VM *vm) {
  if (vm->exception == W_NIL_VALUE) {
    explode("handler not set");
  }
  return vm->exception;
}

void clearException(VM *vm) {
  vm->exception = W_NIL_VALUE;
}

// exception handling

Value _exceptionMake(VM *vm, ExceptionParams p) {

  Array *protectedFrames;
  {
    uint64_t numFrames = 0;
    if (vm->current != NULL) {
      Frame_t current = vm->current;
      while (true) {
        numFrames++;
        if (!hasParent(current)) {
          break;
        }
        else {
          current = getParent(current);
        }
      }
    }

    if (p.raised != NULL) {
      // native frame
      numFrames++;
    }

    protectedFrames = makeArray(vm, numFrames);
    pushFrameRoot(vm, (Value*)&protectedFrames);
  }

  Value protectedFunctionKw = getKeyword(vm, L"function-name");
  pushFrameRoot(vm, (Value*)&protectedFunctionKw);

  Value protectedUnknownSourceKw = getKeyword(vm, L"unknown-source");
  pushFrameRoot(vm, (Value*)&protectedUnknownSourceKw);

  Value protectedFileNameKw = getKeyword(vm, L"file-name");
  pushFrameRoot(vm, (Value*)&protectedFileNameKw);

  Value protectedLineNumberKw = getKeyword(vm, L"line-number");
  pushFrameRoot(vm, (Value*)&protectedLineNumberKw);

  if (p.raised != NULL) { // native frame

    String *protectedFunctionName = (String*) makeString(vm, strlen(p.raised->functionName) + 1);
    swprintf(stringValue(protectedFunctionName), protectedFunctionName->length, L"%s", p.raised->functionName);
    pushFrameRoot(vm, (Value*)&protectedFunctionName);

    Value unknownSource = wrapBool(false);

    char* fileName = basename((char *) p.raised->fileName);
    String *protectedFileName = (String*) makeString(vm, strlen(fileName) + 1);
    swprintf(stringValue(protectedFileName), protectedFileName->length, L"%s", fileName);
    pushFrameRoot(vm, (Value*)&protectedFileName);

    Value lineNumber = wrapUint(p.raised->lineNumber);

    Map *protectedFrame = makeMap(vm);
    pushFrameRoot(vm, (Value*)&protectedFrame);

    putMapEntry(vm, &protectedFrame, protectedFunctionKw, (Value)protectedFunctionName);
    putMapEntry(vm, &protectedFrame, protectedUnknownSourceKw, unknownSource);
    putMapEntry(vm, &protectedFrame, protectedFileNameKw, (Value)protectedFileName);
    putMapEntry(vm, &protectedFrame, protectedLineNumberKw, lineNumber);

    arrayElements(protectedFrames)[0] = (Value)protectedFrame;

    popFrameRoot(vm); // protectedFrame
    popFrameRoot(vm); // protectedFileName
    popFrameRoot(vm); // protectedFunctionName
  }

  if (vm->current != NULL) {

    uint64_t start = 0;
    if (p.raised != NULL) {
      start += 1;
    }

    Frame_t current = vm->current;
    uint64_t numFrames = objectHeaderSize(protectedFrames->header);
    for (uint64_t i = start; i < numFrames; i++) {

      Map *protectedFrame = makeMap(vm);
      pushFrameRoot(vm, (Value*)&protectedFrame);

      Value protectedFnName;
      {
        wchar_t *fnName;
        if (hasFnName(current)) {
          fnName = getFnName(current);
        } else {
          fnName = L"<root>\0";
        }
        protectedFnName = makeStringValue(vm, fnName, wcslen(fnName));
        pushFrameRoot(vm, &protectedFnName);
        putMapEntry(vm, &protectedFrame, protectedFunctionKw, protectedFnName);
        popFrameRoot(vm); //protectedFnName
      }

      if (hasSourceTable(current)) {

        putMapEntry(vm, &protectedFrame, protectedUnknownSourceKw, wrapBool(false));

        {
          wchar_t *fileName = getFileName(current);
          Value protectedFileName = makeStringValue(vm, fileName, wcslen(fileName));
          pushFrameRoot(vm, &protectedFileName);
          putMapEntry(vm, &protectedFrame, protectedFileNameKw, protectedFileName);
          popFrameRoot(vm); //protectedFileName
        }

        uint64_t lineNumber;
        getLineNumber(current, &lineNumber);
        putMapEntry(vm, &protectedFrame, protectedLineNumberKw, wrapUint(lineNumber));
      }
      else {
        putMapEntry(vm, &protectedFrame, protectedUnknownSourceKw, wrapBool(true));
      }

      arrayElements(protectedFrames)[i] = (Value)protectedFrame;
      popFrameRoot(vm); // protectedFrame

      if (hasParent(current)) {
        current = getParent(current);
      }
    }
  }

  Map *protectedExn = makeMap(vm);
  pushFrameRoot(vm, (Value*)&protectedExn);

  if (p.protectedMessage != NULL) {
    putMapEntry(vm, &protectedExn, getKeyword(vm, L"message"), *p.protectedMessage);
  }
  if (p.protectedValue != NULL) {
    putMapEntry(vm, &protectedExn, getKeyword(vm, L"value"), *p.protectedValue);
  }
  putMapEntry(vm, &protectedExn, getKeyword(vm, L"frames"), (Value)protectedFrames);

  popFrameRoot(vm); // protectedExn

  popFrameRoot(vm); // protectedLineNumberKw
  popFrameRoot(vm); // protectedFileNameKw
  popFrameRoot(vm); // protectedUnknownSourceKw
  popFrameRoot(vm); // protectedFunctionKw
  popFrameRoot(vm); // protectedFrames

  return (Value)protectedExn;
}

void raisedInitContents(Raised *r) {
  r->lineNumber = 0;
  r->functionName = NULL;
  r->fileName = NULL;
}

Value exceptionMakeRaised(VM *vm, Raised *raised) {

  wchar_t msg[ERROR_MSG_LENGTH];
  swprintf(msg, ERROR_MSG_LENGTH, L"unhandled error: %ls", raised->message);
  Value protectedMessage = makeStringValue(vm, msg, wcslen(msg));
  pushFrameRoot(vm, (Value *) &protectedMessage);

  ExceptionParams p;
  p.protectedMessage = &protectedMessage;
  p.protectedValue = NULL;
  p.raised = raised;

  Value v = _exceptionMake(vm, p);

  popFrameRoot(vm); // protectedMessage

  return v;
}

Value exceptionMakeKw(VM *vm, Raised *raised, wchar_t *kwName) {

  Value protectedName = makeStringValue(vm, kwName, wcslen(kwName));
  pushFrameRoot(vm, &protectedName);

  Value protectedValue = keywordIntern(vm, &protectedName);
  pushFrameRoot(vm, &protectedValue);

  ExceptionParams p;
  p.protectedMessage = &protectedName;
  p.protectedValue = &protectedValue;
  p.raised = raised;

  Value v = _exceptionMake(vm, p);

  popFrameRoot(vm); // protectedValue
  popFrameRoot(vm); // protectedName

  return v;
}
