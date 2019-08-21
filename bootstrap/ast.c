#include <stdlib.h>
#include "ast.h"
#include "pool.h"

/*
 * Here is the basic AST implementation.
 */

void formInitContents(Form *expr) {
  expr->type = F_NONE;
  sourceLocationInitContents(&expr->source);
}

Form* formMake(Pool_t pool) {
  Form *expr;
  palloc(pool, expr, sizeof(Form), "Expr");
  formInitContents(expr);
  return expr;
}

Form* stringMake(Pool_t pool, wchar_t *input, uint64_t length) {
  Form *expr = formMake(pool);
  expr->type = F_STRING;
  expr->string.length = length;
  expr->string.value = copyText(pool, input, length);
  expr->source.isSet = false;
  return expr;
}

Form* numberMake(Pool_t pool, uint64_t value) {
  Form *expr = formMake(pool);
  expr->type = F_NUMBER;
  expr->number.value = value;
  expr->source.isSet = false;
  return expr;
}

Form* charMake(Pool_t pool, wchar_t value) {
  Form *expr = formMake(pool);
  expr->type = F_CHAR;
  expr->chr.value = value;
  expr->source.isSet = false;
  return expr;
}

Form* symbolMake(Pool_t pool, wchar_t *name, uint64_t len) {
  Form *expr = formMake(pool);
  expr->type = F_SYMBOL;
  expr->symbol.value = copyText(pool, name, len);
  expr->symbol.length = len;
  expr->source.isSet = false;
  return expr;
}

Form* keywordMake(Pool_t pool, wchar_t *name, uint64_t len) {
  Form *expr = formMake(pool);
  expr->type = F_KEYWORD;
  expr->keyword.length = len;
  expr->keyword.value = copyText(pool, name, len);
  expr->source.isSet = false;
  return expr;
}

Form* booleanMake(Pool_t pool, bool value) {
  Form *expr = formMake(pool);
  expr->type = F_BOOLEAN;
  expr->boolean.value = value;
  expr->source.isSet = false;
  return expr;
}

Form* nilMake(Pool_t pool) {
  Form *expr = formMake(pool);
  expr->type = F_NIL;
  expr->source.isSet = false;
  return expr;
}

// valid for zero length list
void listInitContents(FormList *list) {
  list->length = 0;
  list->head = NULL;
  list->tail = NULL;
}

Form* listMake(Pool_t pool) {
  Form *expr = formMake(pool);
  expr->type = F_LIST;
  listInitContents(&expr->list);
  expr->source.isSet = false;
  return expr;
}

void listAppend(Pool_t pool, FormList *list, Form *expr) {
  ListElement *elem;
  palloc(pool, elem, sizeof(ListElement), "ExprList");

  elem->expr = expr;
  elem->next = NULL;

  if (list->head == NULL) { // no elements
    list->head = elem;
    list->tail = elem;
  }
  else if (list->head == list->tail) { // one element
    list->head->next = elem;
    list->tail = elem;
  }
  else { // more than one element
    list->tail->next = elem;
    list->tail = elem;
  }

  list->length = list->length + 1;
}

// vectors

void vecInitContents(FormVec *vec) {
  vec->length = 0;
  vec->head = NULL;
  vec->tail = NULL;
}

Form* vecMake(Pool_t pool) {
  Form *expr = formMake(pool);
  expr->type = F_VEC;
  vecInitContents(&expr->vec);
  expr->source.isSet = false;
  return expr;
}

void vecAppend(Pool_t pool, FormVec *vec, Form *expr) {
  ListElement *elem;
  palloc(pool, elem, sizeof(ListElement), "ListElement");

  elem->expr = expr;
  elem->next = NULL;

  if (vec->head == NULL) { // no elements
    vec->head = elem;
    vec->tail = elem;
  }
  else if (vec->head == vec->tail) { // one element
    vec->head->next = elem;
    vec->tail = elem;
  }
  else { // more than one element
    vec->tail->next = elem;
    vec->tail = elem;
  }

  vec->length = vec->length + 1;
}

// maps

void mapInitContents(FormMap *map) {
  map->length = 0;
  map->head = NULL;
  map->tail = NULL;
}

void mapElementInitContents(MapElement *e) {
  e->key = NULL;
  e->value = NULL;
  e->next = NULL;
}

Form* mapMake(Pool_t pool) {
  Form *expr = formMake(pool);
  expr->type = F_MAP;
  mapInitContents(&expr->map);
  expr->source.isSet = false;
  return expr;
}

void mapPut(Pool_t pool, FormMap *map, Form *key, Form *value) {
  MapElement *elem;
  palloc(pool, elem, sizeof(MapElement), "MapElement");

  mapElementInitContents(elem);

  elem->key = key;
  elem->value = value;

  if (map->head == NULL) { // no elements
    map->head = elem;
    map->tail = elem;
  }
  else if (map->head == map->tail) { // one element
    map->head->next = elem;
    map->tail = elem;
  }
  else { // more than one element
    map->tail->next = elem;
    map->tail = elem;
  }

  map->length = map->length + 1;
}

void exprPrnBufConf(Form *expr, StringBuffer_t b, bool readable) {
  switch (expr->type) {
    case F_NIL:
      stringBufferAppendStr(b, L"nil");
      break;
    case F_NUMBER: {
      wchar_t text[256];
      swprintf(text, sizeof(text), L"%" PRIu64, expr->number.value);
      stringBufferAppendStr(b, text);
      break;
    }
    case F_CHAR: {
      stringBufferAppendChar(b, L'\'');
      stringBufferAppendChar(b, expr->chr.value);
      stringBufferAppendChar(b, L'\'');
      break;
    }
    case F_BOOLEAN:
      if (expr->boolean.value == 0) {
        stringBufferAppendStr(b, L"false");
      }
      else {
        stringBufferAppendStr(b, L"true");
      }
      break;
    case F_STRING: {
      if (readable) {
        stringBufferAppendChar(b, L'"');
        stringBufferAppendStr(b, expr->string.value);
        stringBufferAppendChar(b, L'"');
      }
      else {
        stringBufferAppendStr(b, expr->string.value);
      }
      break;
    }
    case F_SYMBOL: {
      stringBufferAppendStr(b, expr->symbol.value);
      break;
    }
    case F_KEYWORD: {
      stringBufferAppendChar(b, L':');
      stringBufferAppendStr(b, expr->keyword.value);
      break;
    }
    case F_LIST: {
      stringBufferAppendChar(b, L'(');

      ListElement *elem = expr->list.head;
      for (int i=0; i<expr->list.length; i++) {

        exprPrnBufConf(elem->expr, b, readable);

        if (i + 1 < expr->list.length) {
          stringBufferAppendChar(b, L' ');
        }

        elem = elem->next;
      }

      stringBufferAppendChar(b, L')');
      break;
    }
    case F_VEC: {
      stringBufferAppendChar(b, L'[');

      ListElement *elem = expr->vec.head;
      for (int i=0; i<expr->vec.length; i++) {

        exprPrnBufConf(elem->expr, b, readable);

        if (i + 1 < expr->vec.length) {
          stringBufferAppendChar(b, L' ');
        }

        elem = elem->next;
      }

      stringBufferAppendChar(b, L']');
      break;
    }
    case F_MAP: {
      stringBufferAppendChar(b, L'{');

      MapElement *elem = expr->map.head;
      for (int i=0; i<expr->map.length; i++) {

        exprPrnBufConf(elem->key, b, readable);
        stringBufferAppendChar(b, L' ');
        exprPrnBufConf(elem->value, b, readable);

        if (i + 1 < expr->map.length) {
          stringBufferAppendChar(b, L' ');
        }

        elem = elem->next;
      }

      stringBufferAppendChar(b, L'}');
      break;
    }
    default:
    explode("unsuported value type: %u", expr->type);
  }
}

void exprPrnBuf(Form *expr, StringBuffer_t b) {
  return exprPrnBufConf(expr, b, true);
}

wchar_t* exprPrnStr(Pool_t pool, Form *expr) {
  StringBuffer_t b = stringBufferMake(pool);
  exprPrnBuf(expr, b);
  return copyText(pool, stringBufferText(b), stringBufferLength(b));
}

void exprPrn(Pool_t pool, Form *expr) {
  wchar_t *str = exprPrnStr(pool, expr);
  printf("%ls", str);
}

/*
Form* formMake(Pool_t pool) {
  Form *form;
  palloc(pool, form, sizeof(Form), "Form");
  return form;
}

Form* stringMake(Pool_t pool, wchar_t *input, uint64_t length) {
  Form *form = formMake(pool);
  form->type = F_STRING;
  form->string.length = length;
  form->string.value = copyText(pool, input, length);
  form->source.isSet = false;
  return form;
}

Form* numberMake(Pool_t pool, uint64_t value) {
  Form *form = formMake(pool);
  form->type = F_NUMBER;
  form->number.value = value;
  form->source.isSet = false;
  return form;
}

Form* charMake(Pool_t pool, wchar_t value) {
  Form *form = formMake(pool);
  form->type = F_CHAR;
  form->chr.value = value;
  form->source.isSet = false;
  return form;
}

Form* symbolMake(Pool_t pool, wchar_t *name, uint64_t len) {
  Form *form = formMake(pool);
  form->type = F_SYMBOL;
  form->symbol.value = copyText(pool, name, len);
  form->symbol.length = len;
  form->source.isSet = false;
  return form;
}

Form* keywordMake(Pool_t pool, wchar_t *name, uint64_t len) {
  Form *form = formMake(pool);
  form->type = F_KEYWORD;
  form->keyword.value = copyText(pool, name, len);
  form->keyword.length = len;
  form->source.isSet = false;
  return form;
}

Form* booleanMake(Pool_t pool, bool value) {
  Form *form = formMake(pool);
  form->type = F_BOOLEAN;
  form->boolean.value = value;
  form->source.isSet = false;
  return form;
}

Form* nilMake(Pool_t pool) {
  Form *form = formMake(pool);
  form->type = F_NIL;
  form->source.isSet = false;
  return form;
}

Form* listMake(Pool_t pool) {
  Form *form = formMake(pool);
  form->type = F_LIST;
  form->list.length = 0;

  // valid for zero length list
  form->list.head = NULL;
  form->list.tail = NULL;

  form->source.isSet = false;

  return form;
}

void listAppend(Pool_t pool, FormList *list, Form *form) {

  ListElement *elem;
  palloc(pool, elem, sizeof(ListElement), "ExprList");

  elem->expr = form;
  elem->next = NULL;

  if (list->head == NULL) { // no elements
    list->head = elem;
    list->tail = elem;
  }
  else if (list->head == list->tail) { // one element
    list->head->next = elem;
    list->tail = elem;
  }
  else { // more than one element
    list->tail->next = elem;
    list->tail = elem;
  }

  list->length = list->length + 1;
}

Form* vecMake(Pool_t pool) {
  Form *form = formMake(pool);
  form->type = F_VEC;
  form->list.length = 0;

  // valid for zero length list
  form->list.head = NULL;
  form->list.tail = NULL;

  form->source.isSet = false;

  return form;
}

void vecAppend(Pool_t pool, FormVec *vec, Form *form) {

  ListElement *elem;
  palloc(pool, elem, sizeof(ListElement), "ListElement");

  elem->expr = form;
  elem->next = NULL;

  if (vec->head == NULL) { // no elements
    vec->head = elem;
    vec->tail = elem;
  }
  else if (vec->head == vec->tail) { // one element
    vec->head->next = elem;
    vec->tail = elem;
  }
  else { // more than one element
    vec->tail->next = elem;
    vec->tail = elem;
  }

  vec->length = vec->length + 1;
}

Form* mapMake(Pool_t pool, Error *error) {
  Form *form = formMake(pool);
  form->type = F_MAP;
  form->source.isSet = false;

  // valid for zero length map
  mapInitContents(&form->map);
  return form;
}

*/
