#include <stdlib.h>
#include <inttypes.h>
#include "print.h"
#include "vm.h"
#include "errors.h"

/*
 * Create a reader representation of a vm Value (an Expr).
 *
 * Some representations are approximate and cannot be round-tripped through eval, such as functions and closures.
 */

typedef struct Ctx {
  VM_t vm;
  Pool_t pool;
} Ctx;

void _print(Ctx *ctx, Value result, Expr *expr);

typedef void (*PrintGeneric) (Ctx *ctx, Value result, Expr *expr);

bool isEmpty(Value value) {
  return valueType(value) == VT_NIL;
}

void printNil(Ctx *ctx, Value result, Expr *expr) {
  expr->type = N_NIL;
}

void printUint(Ctx *ctx, Value result, Expr *expr) {
  expr->type = N_NUMBER;
  expr->number.value = unwrapUint(result);
}

void printBool(Ctx *ctx, Value result, Expr *expr) {
  expr->type = N_BOOLEAN;
  expr->boolean.value = unwrapBool(result);
}

void printFn(Ctx *ctx, Value result, Expr *expr) {
  expr->type = N_STRING;
  wchar_t function[] = L"<function>";
  expr->string.length = wcslen(function);

  Error error;
  errorInitContents(&error);
  if (tryCopyText(ctx->pool, function, &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void printCFn(Ctx *ctx, Value result, Expr *expr) {
  expr->type = N_STRING;
  wchar_t function[] = L"<c-function>";
  expr->string.length = wcslen(function);

  Error error;
  errorInitContents(&error);
  if (tryCopyText(ctx->pool, function, &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void printClosure(Ctx *ctx, Value result, Expr *expr) {
  expr->type = N_STRING;
  wchar_t function[] = L"<closure>";
  expr->string.length = wcslen(function);

  Error error;
  errorInitContents(&error);
  if (tryCopyText(ctx->pool, function, &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void printStr(Ctx *ctx, Value result, Expr *expr) {
  String *str = deref(ctx->vm, result);
  expr->type = N_STRING;
  expr->string.length = str->length;

  Error error;
  errorInitContents(&error);
  if(tryCopyText(ctx->pool, stringValue(str), &expr->string.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void printSymbol(Ctx *ctx, Value result, Expr *expr) {
  Symbol *sym = deref(ctx->vm, result);
  String *str = deref(ctx->vm, sym->name);
  expr->type = N_SYMBOL;
  expr->symbol.length = str->length;

  Error error;
  errorInitContents(&error);
  if(tryCopyText(ctx->pool, stringValue(str), &expr->symbol.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
}

void printKeyword(Ctx *ctx, Value result, Expr *expr) {
  Keyword *kw = deref(ctx->vm, result);
  String *str = deref(ctx->vm, kw->name);
  expr->type = N_KEYWORD;
  expr->keyword.length = str->length;

  Error error;
  errorInitContents(&error);
  if (tryCopyText(ctx->pool, stringValue(str), &expr->keyword.value, expr->string.length, &error) != R_SUCCESS) {
    explode("copy text");
  }
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

void printMetadata(VM_t vm, Value metadata, Expr *expr) {
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

void printList(Ctx *ctx, Value result, Expr *expr) {
  Cons *cons = deref(ctx->vm, result);

  expr->type = N_LIST;

  printMetadata(ctx->vm, cons->metadata, expr);

  listInitContents(&expr->list);
  Expr *elem;

  palloc(ctx->pool, elem, sizeof(Expr), "Expr");
  exprInitContents(elem);

  _print(ctx, cons->value, elem);

  Error error;
  errorInitContents(&error);
  if (tryListAppend(ctx->pool, &expr->list, elem, &error) != R_SUCCESS) {
    explode("list append");
  }

  while (valueType(cons->next) != VT_NIL) {

    if (valueType(cons->next) != VT_LIST) {
      explode("this should always be a type of VT_LIST: %s",
              getValueTypeName(ctx->vm, valueType(cons->next)));
    }

    cons = deref(ctx->vm, cons->next);

    palloc(ctx->pool, elem, sizeof(Expr), "Expr");
    exprInitContents(elem);

    _print(ctx, cons->value, elem);

    if (tryListAppend(ctx->pool, &expr->list, elem, &error) != R_SUCCESS) {
      explode("list append");
    }
  }
}

void printMap(Ctx *ctx, Value result, Expr *expr) {
  Map *map = deref(ctx->vm, result);
  Array *array = deref(ctx->vm, map->entries);

  expr->type = N_MAP;
  mapInitContents(&expr->map);

  uint64_t size = objectHeaderSize(array->header);
  for (uint64_t i=0; i<size; i++) {
    Value entryRef = arrayElements(array)[i];

    if (valueType(entryRef) == VT_MAP_ENTRY) {

      MapEntry *entry = deref(ctx->vm, entryRef);
      if (entry->used) {

        Expr *keyExpr = NULL;
        palloc(ctx->pool, keyExpr, sizeof(Expr), "Expr");
        exprInitContents(keyExpr);
        _print(ctx, entry->key, keyExpr);

        Expr *valueExpr = NULL;
        palloc(ctx->pool, valueExpr, sizeof(Expr), "Expr");
        exprInitContents(valueExpr);
        _print(ctx, entry->value, valueExpr);

        Error e;
        errorInitContents(&e);
        if (tryMapPut(ctx->pool, &expr->map, keyExpr, valueExpr, &e) != R_SUCCESS) {
          explode("put");
        }
      }
    }
  }
}

void printArray(Ctx *ctx, Value result, Expr *expr) {

  Array *array = deref(ctx->vm, result);
  uint64_t size = objectHeaderSize(array->header);
  Value *elements = arrayElements(array);

  expr->type = N_VEC;
  vecInitContents(&expr->vec);

  Error error;
  errorInitContents(&error);

  for (uint64_t i=0; i<size; i++) {

    Expr *elem = NULL;
    palloc(ctx->pool, elem, sizeof(Expr), "Expr");
    exprInitContents(elem);

    _print(ctx, elements[i], elem);

    if (tryVecAppend(ctx->pool, &expr->vec, elem, &error) != R_SUCCESS) {
      explode("list append");
    }
  }
}

void printRecord(Ctx *ctx, Value result, Expr *expr) {
  Record *record = deref(ctx->vm, result);

  Error error;
  errorInitContents(&error);

  StringBuffer_t b = NULL;
  if (tryStringBufferMake(ctx->pool, &b, &error) != R_SUCCESS) {
    explode("sbmake");
  }

  if (tryStringBufferAppendStr(b, L"#", &error) != R_SUCCESS) {
    explode("append");
  }

  Symbol *symbol = deref(ctx->vm, record->symbol);
  String *name = deref(ctx->vm, symbol->name);

  if (tryStringBufferAppendStr(b, stringValue(name), &error) != R_SUCCESS) {
    explode("append");
  }

  if (tryStringBufferAppendStr(b, L"[", &error) != R_SUCCESS) {
    explode("append");
  }

  if (tryStringBufferAppendStr(b, L"]", &error) != R_SUCCESS) {
    explode("append");
  }

  expr->type = N_STRING;
  expr->string.length = stringBufferLength(b);
  expr->string.value = stringBufferText(b);
}

PrintGeneric getPrintGeneric(Ctx *ctx, ValueType type) {
  switch (type) {
    case VT_NIL:       return printNil;
    case VT_UINT:      return printUint;
    case VT_BOOL:      return printBool;
    case VT_FN:        return printFn;
    case VT_STR:       return printStr;
    case VT_SYMBOL:    return printSymbol;
    case VT_KEYWORD:   return printKeyword;
    case VT_LIST:      return printList;
    case VT_CLOSURE:   return printClosure;
    case VT_CFN:       return printCFn;
    case VT_ARRAY:     return printArray;
    case VT_MAP:       return printMap;
    case VT_RECORD:    return printRecord;
    default: explode("unhandled type: %s", getValueTypeName(ctx->vm, type));
  }
}

void _print(Ctx *ctx, Value result, Expr *expr) {
  ValueType type = valueType(result);
  exprInitContents(expr);
  PrintGeneric p = getPrintGeneric(ctx, type);
  p(ctx, result, expr);
}

Expr* printToReader(VM_t vm, Pool_t pool, Value result) {
  Ctx ctx;
  ctx.vm = vm;
  ctx.pool= pool;

  Expr *elem;
  palloc(pool, elem, sizeof(Expr), "Expr");
  exprInitContents(elem);

  _print(&ctx, result, elem);
  return elem;
}

#define ONE_KB 1024

void print(VM_t vm, Value value) {
  Error error;
  errorInitContents(&error);

  Pool_t pool = NULL;
  if (tryPoolCreate(&pool, ONE_KB, &error) != R_SUCCESS) {
    explode("oops");
  }

  Ctx ctx;
  ctx.vm = vm;
  ctx.pool = pool;

  Expr elem;
  exprInitContents(&elem);
  _print(&ctx, value, &elem);

  if (tryExprPrn(pool, &elem, &error) != R_SUCCESS) {
    explode("oops");
  }

  poolFree(pool);
}

wchar_t* printExceptionValue(Ctx *ctx, Value e) {

  Map *exn = deref(ctx->vm, e);

  String *message = (String*)mapLookup(ctx->vm, exn, getKeyword(ctx->vm, L"message"));
  Array *frames = (Array*)mapLookup(ctx->vm, exn, getKeyword(ctx->vm, L"frames"));

  Error error;
  errorInitContents(&error);

  StringBuffer_t b = NULL;

  if (tryStringBufferMake(ctx->pool, &b, &error) != R_SUCCESS) {
    explode("oops");
  }

  if (tryStringBufferAppendStr(b, stringValue(message), &error) != R_SUCCESS) {
    explode("oops");
  }

  if (tryStringBufferAppendStr(b, L"\n", &error) != R_SUCCESS) {
    explode("oops");
  }

  uint64_t numFrames = objectHeaderSize(frames->header);
  Value* elements = arrayElements(frames);

  for (uint64_t i=0; i<numFrames; i++) {
    Map *frame = deref(ctx->vm, elements[i]);

    String *functionName = (String*)mapLookup(ctx->vm, frame, getKeyword(ctx->vm, L"function-name"));
    bool unknownSource = unwrapBool(mapLookup(ctx->vm, frame, getKeyword(ctx->vm, L"unknown-source")));

    wchar_t msg[ERROR_MSG_LENGTH];
    if (unknownSource) {
      swprintf(msg, ERROR_MSG_LENGTH, L"\tat %ls(Unknown Source)\n", stringValue(functionName));
    }
    else {
      String *fileName = (String*)mapLookup(ctx->vm, frame, getKeyword(ctx->vm, L"file-name"));
      uint64_t lineNumber = unwrapUint(mapLookup(ctx->vm, frame, getKeyword(ctx->vm, L"line-number")));
      swprintf(msg, ERROR_MSG_LENGTH, L"\tat %ls(%ls:%" PRIu64 ")\n",
          stringValue(functionName), stringValue(fileName), lineNumber);
    }

    if (tryStringBufferAppendStr(b, msg, &error) != R_SUCCESS) {
      explode("oops");
    }
  }

  wchar_t *output;
  if (tryCopyText(ctx->pool, stringBufferText(b), &output, stringBufferLength(b), &error) != R_SUCCESS) {
    explode("oops");
  }

  return output;
}

void printException(VM_t vm, Value exception) {
  Error error;
  errorInitContents(&error);

  Pool_t pool = NULL;
  if (tryPoolCreate(&pool, ONE_KB, &error) != R_SUCCESS) {
    explode("oops");
  }

  Ctx ctx;
  ctx.vm = vm;
  ctx.pool= pool;

  wchar_t *msg = printExceptionValue(&ctx, exception);
  printf("%ls\n", msg);

  poolFree(pool);
}
