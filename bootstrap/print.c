#include <stdlib.h>
#include <inttypes.h>
#include "print.h"
#include "../vm/vm.h"
#include "../errors.h"

/*
 * Create a reader representation of a vm Value (an Expr).
 *
 * Some representations are approximate and cannot be round-tripped through eval, such as functions and closures.
 */

typedef struct Ctx {
  VM_t vm;
  Pool_t pool;
} Ctx;

void _print(Ctx *ctx, Value result, Form *expr);

typedef void (*PrintGeneric) (Ctx *ctx, Value result, Form *expr);

bool isEmpty(Value value) {
  return valueType(value) == VT_NIL;
}

void printNil(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_NIL;
}

void printUint(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_NUMBER;
  expr->number.value = unwrapUint(result);
}

void printChar(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_CHAR;
  expr->chr.value = unwrapChar(result);
}

void printBool(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_BOOLEAN;
  expr->boolean.value = unwrapBool(result);
}

void printFn(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_STRING;
  wchar_t function[] = L"<function>";
  expr->string.length = wcslen(function);
  expr->string.value = copyText(ctx->pool, function, expr->string.length);
}

void printCFn(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_STRING;
  wchar_t function[] = L"<c-function>";
  expr->string.length = wcslen(function);
  expr->string.value = copyText(ctx->pool, function, expr->string.length);
}

void printClosure(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_STRING;
  wchar_t function[] = L"<closure>";
  expr->string.length = wcslen(function);
  expr->string.value = copyText(ctx->pool, function, expr->string.length);
}

void printStr(Ctx *ctx, Value result, Form *expr) {
  String *str = deref(ctx->vm, result);
  expr->type = F_STRING;
  expr->string.length = str->length;
  expr->string.value = copyText(ctx->pool, stringValue(str), expr->string.length);
}

void printSymbol(Ctx *ctx, Value result, Form *expr) {
  Symbol *sym = deref(ctx->vm, result);
  String *str = deref(ctx->vm, sym->name);
  expr->type = F_SYMBOL;
  expr->symbol.length = str->length;
  expr->symbol.value = copyText(ctx->pool, stringValue(str), expr->string.length);
}

void printKeyword(Ctx *ctx, Value result, Form *expr) {
  Keyword *kw = deref(ctx->vm, result);
  String *str = deref(ctx->vm, kw->name);
  expr->type = F_KEYWORD;
  expr->keyword.length = str->length;
  expr->keyword.value = copyText(ctx->pool, stringValue(str), expr->string.length);
}

typedef struct Property {
  Keyword *key;
  Value value;
} Property;

void readProperty(VM_t vm, Value *ptr, Property *p) {

  if (valueType(*ptr) != VT_LIST) {
    explode("expected property list: %s",
            getValueTypeName(vm, valueType(*ptr)));
  }

  Cons *properties = deref(vm, *ptr);

  if (valueType(properties->value) != VT_KEYWORD) {
    explode("expected keyword for property key: %s",
            getValueTypeName(vm, valueType(properties->value)));
  }

  p->key = deref(vm, properties->value);

  if (isEmpty(properties->next)) {
    String *str = deref(vm, p->key->name);
    explode("expected value for property but only found a key: %ls", stringValue(str));
  }

  properties = deref(vm, properties->next);
  p->value = properties->value;

  *ptr = properties->next;
}

void printMetadata(VM_t vm, Value metadata, Form *expr) {
  while (!isEmpty(metadata)) {

    Property p;
    readProperty(vm, &metadata, &p);

    String *str = deref(vm, p.key->name);

    if (wcscmp(L"line-number", stringValue(str)) == 0) {

      if (valueType(p.value) != VT_UINT) {
        explode("expected line-number property value to be an int: %s",
                getValueTypeName(vm, valueType(p.value)));
      }

      expr->source.isSet = true;
      expr->source.lineNumber = unwrapUint(p.value);
    }
    else {
      // ignore property
    }
  }
}

void printList(Ctx *ctx, Value result, Form *expr) {
  Cons *cons = deref(ctx->vm, result);

  expr->type = F_LIST;

  printMetadata(ctx->vm, cons->metadata, expr);

  listInitContents(&expr->list);
  Form *elem;

  palloc(ctx->pool, elem, sizeof(Form), "Expr");
  formInitContents(elem);

  _print(ctx, cons->value, elem);

  Error error;
  errorInitContents(&error);
  printMetadata(ctx->vm, cons->metadata, elem);
  listAppend(ctx->pool, &expr->list, elem);

  while (valueType(cons->next) != VT_NIL) {

    if (valueType(cons->next) != VT_LIST) {
      explode("this should always be a type of VT_LIST: %s",
              getValueTypeName(ctx->vm, valueType(cons->next)));
    }

    cons = deref(ctx->vm, cons->next);

    palloc(ctx->pool, elem, sizeof(Form), "Expr");
    formInitContents(elem);

    _print(ctx, cons->value, elem);
    printMetadata(ctx->vm, cons->metadata, elem);

    listAppend(ctx->pool, &expr->list, elem);
  }
}

void printMap(Ctx *ctx, Value result, Form *expr) {
  Map *map = deref(ctx->vm, result);
  Array *array = deref(ctx->vm, map->entries);

  expr->type = F_MAP;
  mapInitContents(&expr->map);

  uint64_t size = objectHeaderSize(array->header);
  for (uint64_t i=0; i<size; i++) {
    Value entryRef = arrayElements(array)[i];

    if (valueType(entryRef) == VT_MAP_ENTRY) {

      MapEntry *entry = deref(ctx->vm, entryRef);
      if (entry->used) {

        Form *keyExpr = NULL;
        palloc(ctx->pool, keyExpr, sizeof(Form), "Expr");
        formInitContents(keyExpr);
        _print(ctx, entry->key, keyExpr);

        Form *valueExpr = NULL;
        palloc(ctx->pool, valueExpr, sizeof(Form), "Expr");
        formInitContents(valueExpr);
        _print(ctx, entry->value, valueExpr);

        Error e;
        errorInitContents(&e);
        mapPut(ctx->pool, &expr->map, keyExpr, valueExpr);
      }
    }
  }
}

void printArray(Ctx *ctx, Value result, Form *expr) {

  Array *array = deref(ctx->vm, result);
  uint64_t size = objectHeaderSize(array->header);
  Value *elements = arrayElements(array);

  expr->type = F_VEC;
  vecInitContents(&expr->vec);

  Error error;
  errorInitContents(&error);

  for (uint64_t i=0; i<size; i++) {

    Form *elem = NULL;
    palloc(ctx->pool, elem, sizeof(Form), "Expr");
    formInitContents(elem);

    _print(ctx, elements[i], elem);

    vecAppend(ctx->pool, &expr->vec, elem);
  }
}

void printRecord(Ctx *ctx, Value result, Form *expr) {
  Record *record = deref(ctx->vm, result);

  Error error;
  errorInitContents(&error);

  StringBuffer_t b = stringBufferMake(ctx->pool);

  stringBufferAppendStr(b, L"#record");
  stringBufferAppendStr(b, L"[");
  stringBufferAppendStr(b, L"]");

  expr->type = F_STRING;
  expr->string.length = stringBufferLength(b);
  expr->string.value = stringBufferText(b);
}

void printPort(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_STRING;
  wchar_t function[] = L"<port>";
  expr->string.length = wcslen(function);
  expr->string.value = copyText(ctx->pool, function, expr->string.length);
}

void printByteArray(Ctx *ctx, Value result, Form *expr) {
  expr->type = F_STRING;
  wchar_t function[] = L"<byte-array>";
  expr->string.length = wcslen(function);
  expr->string.value = copyText(ctx->pool, function, expr->string.length);
}

PrintGeneric getPrintGeneric(Ctx *ctx, ValueType type) {
  switch (type) {
    case VT_NIL:        return printNil;
    case VT_UINT:       return printUint;
    case VT_CHAR:       return printChar;
    case VT_BOOL:       return printBool;
    case VT_FN:         return printFn;
    case VT_STR:        return printStr;
    case VT_SYMBOL:     return printSymbol;
    case VT_KEYWORD:    return printKeyword;
    case VT_LIST:       return printList;
    case VT_CLOSURE:    return printClosure;
    case VT_CFN:        return printCFn;
    case VT_ARRAY:      return printArray;
    case VT_MAP:        return printMap;
    case VT_RECORD:     return printRecord;
    case VT_PORT:       return printPort;
    case VT_BYTE_ARRAY: return printByteArray;
    default: explode("unhandled type: %s", getValueTypeName(ctx->vm, type));
  }
}

void _print(Ctx *ctx, Value result, Form *expr) {
  ValueType type = valueType(result);
  formInitContents(expr);
  PrintGeneric p = getPrintGeneric(ctx, type);
  p(ctx, result, expr);
}

Form* printToReader(VM_t vm, Pool_t pool, Value result) {
  Ctx ctx;
  ctx.vm = vm;
  ctx.pool= pool;

  Form *elem = formMake(pool);
  _print(&ctx, result, elem);

  return elem;
}

#define ONE_MB (1024 * 1000)

void print(VM_t vm, Value value) {
  Error error;
  errorInitContents(&error);

  Pool_t pool = poolCreate(ONE_MB);

  Ctx ctx;
  ctx.vm = vm;
  ctx.pool = pool;

  Form elem;
  formInitContents(&elem);
  _print(&ctx, value, &elem);

  exprPrn(pool, &elem);

  poolFree(pool);
}

void printBuf(Ctx *ctx, StringBuffer_t b, Value value) {
  Error error;
  errorInitContents(&error);

  Form elem;
  formInitContents(&elem);
  _print(ctx, value, &elem);

  wchar_t *str = exprPrnStr(ctx->pool, &elem);
  stringBufferAppendStr(b, str);
}

wchar_t* printExceptionValue(Ctx *ctx, Value e) {

  Map *exn = deref(ctx->vm, e);
  pushFrameRoot(ctx->vm, (Value*)&exn);

  Value messageKw = getKeyword(ctx->vm, L"message");
  pushFrameRoot(ctx->vm, &messageKw);

  Value valueKw = getKeyword(ctx->vm, L"value");
  pushFrameRoot(ctx->vm, &valueKw);

  Value framesKw = getKeyword(ctx->vm, L"frames");
  pushFrameRoot(ctx->vm, &framesKw);

  Value functionNameKw = getKeyword(ctx->vm, L"function-name");
  pushFrameRoot(ctx->vm, &functionNameKw);

  Value unknownSourceKw = getKeyword(ctx->vm, L"unknown-source");
  pushFrameRoot(ctx->vm, &unknownSourceKw);

  Value fileNameKw = getKeyword(ctx->vm, L"file-name");
  pushFrameRoot(ctx->vm, &fileNameKw);

  Value lineNumberKw = getKeyword(ctx->vm, L"line-number");
  pushFrameRoot(ctx->vm, &lineNumberKw);

  Value message = mapLookup(ctx->vm, exn, messageKw);
  Value value = mapLookup(ctx->vm, exn, valueKw);
  Array *frames = deref(ctx->vm, mapLookup(ctx->vm, exn, framesKw));

  Error error;
  errorInitContents(&error);

  StringBuffer_t b = NULL;

  b = stringBufferMake(ctx->pool);

  if (message != W_NIL_VALUE && value != W_NIL_VALUE) {
    stringBufferAppendStr(b, stringValue(deref(ctx->vm, message)));
    stringBufferAppendStr(b, L" / ");
    printBuf(ctx, b, value);
  }
  else if (message != W_NIL_VALUE) {
    stringBufferAppendStr(b, stringValue(deref(ctx->vm, message)));
  }
  else if (value != W_NIL_VALUE) {
    printBuf(ctx, b, value);
  }

  stringBufferAppendStr(b, L"\n");

  uint64_t numFrames = objectHeaderSize(frames->header);
  Value* elements = arrayElements(frames);

  for (uint64_t i=0; i<numFrames; i++) {
    Map *frame = deref(ctx->vm, elements[i]);

    String *functionName = (String*)mapLookup(ctx->vm, frame, functionNameKw);
    bool unknownSource = unwrapBool(mapLookup(ctx->vm, frame, unknownSourceKw));

    wchar_t msg[ERROR_MSG_LENGTH];
    if (unknownSource) {
      swprintf(msg, ERROR_MSG_LENGTH, L"\tat %ls(Unknown Source)\n", stringValue(functionName));
    }
    else {
      String *fileName = (String*)mapLookup(ctx->vm, frame, fileNameKw);
      uint64_t lineNumber = unwrapUint(mapLookup(ctx->vm, frame, lineNumberKw));
      swprintf(msg, ERROR_MSG_LENGTH, L"\tat %ls(%ls:%" PRIu64 ")\n",
          stringValue(functionName), stringValue(fileName), lineNumber);
    }

    stringBufferAppendStr(b, msg);
  }

  popFrameRoot(ctx->vm); // message
  popFrameRoot(ctx->vm); // value
  popFrameRoot(ctx->vm); // frames
  popFrameRoot(ctx->vm); // function-name
  popFrameRoot(ctx->vm); // unknown-source
  popFrameRoot(ctx->vm); // file-name
  popFrameRoot(ctx->vm); // line-number

  popFrameRoot(ctx->vm); // exn

  return copyText(ctx->pool, stringBufferText(b), stringBufferLength(b));
}

void printException(VM_t vm, Value exception) {
  Error error;
  errorInitContents(&error);

  Pool_t pool = poolCreate(ONE_MB);

  Ctx ctx;
  ctx.vm = vm;
  ctx.pool= pool;

  wchar_t *msg = printExceptionValue(&ctx, exception);
  printf("%ls\n", msg);

  poolFree(pool);
}
