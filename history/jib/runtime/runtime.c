// TODO: overflows in math

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

typedef struct s_Cons {
  struct s_Value* head;
  struct s_Value* tail;
} Cons;

typedef Value List;

typedef struct s_Fn {
  void* ptr; // the address of the function
} Fn;

//  What we really want is a single contiguous chunk of memory with both the
//  headers and the value. This means the pointer should point directly to the
//  memory after the end of the struct, not the result of a different malloc.

struct s_Value {
  Type type;
  uint64_t data_length;
  void* data;
  bool collectable;
  struct s_Value* next;
};

uint64_t value_length(uint64_t data_length) {
  return sizeof(struct s_Value) + data_length;
}

struct s_Runtime {
  void* max_stack_addr;
  uint64_t max_heap_size;
  uint64_t current_heap_size;
  struct s_Value* root;
};

bool space_available(Runtime rt, uint64_t length) {
  return rt->current_heap_size + length < rt->max_heap_size;
}

void mark_all_collectable(Runtime rt) {
  Value current = rt->root;
  while (current != NULL) {
    current->collectable = true;
    current = current->next;
  }
}

// - recurse through value tree
void mark_not_collectable(Value v) {

  v->collectable = false;

  if (v->type == T_LIST) {

    Value referred = (Value)v->data;
    mark_not_collectable(referred);

    if (referred->type == T_CONS) {
      Cons* cons = (Cons*) referred->data;
      mark_not_collectable(cons->head);
      mark_not_collectable(cons->tail);
    }
  }
}

bool is_value(Runtime rt, Value v) {
  Value current = rt->root;
  while (current != NULL) {
    printf("  inspecting gc value: %p\n", current);
    if (v == current) {
      return true;
    }
    current = current->next;
  }
  return false;
}

// traverse root set and all dependencies, mark things that aren't collectable
void mark(Runtime rt, Context ctx) {

  mark_all_collectable(rt);

  void* cursor = ctx.stack_pointer;
  while (cursor != ctx.base_pointer) {
    Value v = *((Value*)cursor);
    if (is_value(rt, v)) {
      printf("IS a value pointer, marking it not collectable: %p\n", v);
      mark_not_collectable(v);
    }
    else {
      printf("not a value pointer: %p\n", v);
    }
    cursor = cursor + 8;
  }

  if (ctx.base_pointer < rt->max_stack_addr) {
    ctx.stack_pointer = ctx.base_pointer + 16;
    ctx.base_pointer =  *((void**)ctx.base_pointer);
    mark(rt, ctx);
  }
}

void free_value(struct s_Value* v);

// remove already-collectable items, otherwise mark items as collectable
void sweep(Runtime rt) {

  Value prev = NULL;
  Value current = rt->root;
  Value next = NULL;

  uint64_t free_length = 0;
  while (current != NULL) {

    next = current->next;

    if (current->collectable) {

      free_length = value_length(current->data_length);
      printf("collecting: %p (%llu, %i)\n", current, free_length, current->type);

      if (prev == NULL) {
        rt->root = next; // remove root node
      }
      else {
        prev->next = next; // remove child node
      }

      free_value(current);
      rt->current_heap_size -= free_length;
      current = next;
    }
    else {
      printf("NOT collecting: %p (%llu, %i)\n", current, free_length, current->type);
      prev = current;
      current = next;
    }
  }
}

void collect(Runtime rt, Context ctx) {
  mark(rt, ctx);
  sweep(rt);
}

struct s_Value* alloc_value(Runtime rt, Context ctx, Type type, uint64_t data_length) {

  uint64_t length = value_length(data_length);

  if (!space_available(rt, length)) {

    fprintf(stderr, "rt: no space available, collecting...\n");
    
    collect(rt, ctx);

    if (!space_available(rt, length)) {
      fprintf(stderr,
              "rt: no space available after collection, used [%llu], max [%llu], requested [%llu]\n",
              rt->current_heap_size,
              rt->max_heap_size,
              length);
      exit(EXIT_FAILURE);
      return NULL;
    }
  }

  void* ptr = malloc(length);

  if (ptr == NULL) {
    perror("rt:alloc_value:malloc");
    exit(-1);
  }

  struct s_Value* v = (struct s_Value*)ptr;

  v->type = type;
  v->data_length = data_length;
  v->data = ptr + sizeof(struct s_Value);
  v->collectable = true;
  v->next = rt->root;

  rt->root = v;
  rt->current_heap_size += length;

  return v;
}

void free_value(struct s_Value* v) {
  free(v);
}

//// implementing public API ////

Runtime rt_create(void* max_stack_addr, uint64_t max_heap_size) {
  Runtime rt = malloc(sizeof(struct s_Runtime));

  if (rt == NULL) {
    perror("rt_create:malloc");
    exit(-1);
  }

  rt->max_stack_addr = max_stack_addr;
  rt->max_heap_size = max_heap_size;
  rt->current_heap_size = 0;
  rt->root = NULL;

  return rt;
}

void rt_destroy(Runtime rt) {

  struct s_Value* free_me = NULL;
  while (rt->root != NULL) {
    free_me = rt->root;
    rt->root = free_me->next;
    free_value(free_me);
  }
  
  free(rt);
}


/**
 * This is really only for testing from C code. Generated assembly would just
 * supply the correct context values for the rt_alloc_* functions based on
 * current register values.
 */
Context rt_create_parent_frame_context() {

  // capture current base pointer
  void* base_pointer;
  asm volatile ("movq %%rbp, %0" : "=r" (base_pointer));

  Context ctx;
  ctx.stack_pointer = base_pointer + 16; // the parent frame's stack pointer
  ctx.base_pointer = *((void**)base_pointer);      // the parent frame's base pointer

  return ctx;
}

Value rt_alloc_nil(Runtime rt, Context ctx) {
  return alloc_value(rt, ctx, T_NIL, 0);
}

Value rt_alloc_bool(Runtime rt, Context ctx, bool b) {
  struct s_Value* v = alloc_value(rt, ctx, T_BOOL, sizeof(bool));
  bool* ptr = (bool*) v->data;
  *ptr = b;
  return v;
}

Value rt_alloc_str(Runtime rt, Context ctx, char* s) {
  uint64_t len = strlen(s) + 1; // includes null terminator
  struct s_Value* v = alloc_value(rt, ctx, T_STR, len);
  strncpy(v->data, s, len);
  return v;
}

Value rt_alloc_int(Runtime rt, Context ctx, int64_t i) {
  struct s_Value* v = alloc_value(rt, ctx, T_INT, sizeof(int64_t));
  int64_t* ptr = (int64_t*) v->data;
  *ptr = i;
  return v;
}

Value rt_alloc_list(Runtime rt, Context ctx) {
  struct s_Value* v = alloc_value(rt, ctx, T_LIST, sizeof(List));
  v->data = rt_alloc_nil(rt, ctx);
  return v;
}

Value rt_alloc_fn(Runtime rt, Context ctx, void* ptr) {
  struct s_Value* v = alloc_value(rt, ctx, T_FN, sizeof(Fn));
  Fn* fn = (Fn*) v->data;
  fn->ptr = ptr;
  return v;
}

Type rt_val_type(Value v) {
  return v->type;
}

uint64_t rt_val_size(Value v) {
  return value_length(v->data_length);
}

void assert_not_null(Value v) {
  if (v == NULL) {
    fprintf(stderr, "rt: value must not be null");
    exit(EXIT_FAILURE);
  }
}

void assert_type(Value v, Type t) {
  if (v->type != t) {
    fprintf(stderr, "rt: only accepts (type %i) values: %i\n", t, v->type);
    exit(EXIT_FAILURE);
  }
}

void* rt_val_fn_ptr(Value v) {
  assert_type(v, T_FN);
  Fn* fn = (Fn*) v->data;
  return fn->ptr;
}

/**
 * There needs to be a 'list' pointer type.
 * - That pointer type could point to a nil value, meaning the list is empty.
 * - That pointer type could point to a cons value, meaning the list has at least one item in it.
 * - cons values always contain a non-NULL value reference for head.
 * - cons values indicate 'end' by putting a reference to a nil value in *tail*. 
 *
 * list_value[ptr] -> cons_value[x, ptr] -> nil_value
 */
Value rt_val_cons(Runtime rt, Context ctx, Value x, Value xs) {
  assert_not_null(x);
  assert_not_null(xs);

  if (xs->type == T_LIST) {

    Value cons_value = alloc_value(rt, ctx, T_CONS, sizeof(Cons));
    Cons* cons = (Cons*) cons_value->data;
    cons->head = x;
    cons->tail = (Value)xs->data;

    Value new_list = alloc_value(rt, ctx, T_LIST, sizeof(List));
    new_list->data = cons_value;

    return new_list;
  }
  else {
    Value list = alloc_value(rt, ctx, T_LIST, sizeof(List));
    list = rt_val_cons(rt, ctx, xs, list);
    list = rt_val_cons(rt, ctx, x, list);
    return list;
  }
}

Value rt_val_first(Value list) {
  assert_type(list, T_LIST);
  Value referred = (Value)list->data;
  if (referred->type == T_CONS) {
    Cons* cons = (Cons*) referred->data;
    return cons->head;
  }
  else {
    return referred;
  }
}

Value rt_val_rest(Runtime rt, Context ctx, Value list) {
  assert_type(list, T_LIST);

  Value referred = (Value)list->data;
  if (referred->type == T_NIL) {
    return list; // the list is empty, just return it back
  }
  else {
    assert_type(referred, T_CONS);

    Value cons_value = (Value) list->data;
    Cons* cons = (Cons*) cons_value->data;

    Value new_list = alloc_value(rt, ctx, T_LIST, sizeof(List));
    new_list->data = cons->tail;
    return new_list;
  }
}

bool seq(Value v) {

  if (v->type != T_LIST) {
    return false;
  }

  Value list = v;
  Value referred = (Value) list->data;

  if (referred->type == T_NIL) {
    return false;
  }

  assert_type(referred, T_CONS);
  return true;
}

Value rt_val_count(Runtime rt, Context ctx, Value list) {
  assert_type(list, T_LIST);

  int64_t count = 0;
  Value current = (Value)list->data;

  while (current->type != T_NIL) {
    Cons* cons = (Cons*) current->data;
    current = cons->tail;
    count++;
  }
  
  return rt_alloc_int(rt, ctx, count);
}

bool rt_val_equals(Value a, Value b) {

  if (a->type != b->type) {
    return false;
  }

  if (a->type == T_NIL) {
    return true;
  }

  if (a->type == T_BOOL) {
    bool* aptr = (bool*) a->data;
    bool* bptr = (bool*) b->data;
    return *aptr == *bptr;
  }

  if (a->type == T_STR) {
    char* aptr = (char*) a->data;
    char* bptr = (char*) b->data;
    return strcmp(aptr, bptr) == 0;
  }

  if (a->type == T_INT) {
    int64_t* aptr = (int64_t*) a->data;
    int64_t* bptr = (int64_t*) b->data;
    return *aptr == *bptr;
  }

  if (a->type == T_LIST) {

    Value aref = (Value)a->data;
    Value bref = (Value)b->data;

    while (true) {

      if (aref->type != bref->type) {
        return false;
      }

      if (aref->type == T_NIL) {
        return true;
      }

      Cons* acons = (Cons*) aref->data;
      Cons* bcons = (Cons*) bref->data;

      if (!rt_val_equals(acons->head, bcons->head)) {
        return false;
      }

      aref = acons->tail;
      bref = bcons->tail;
    }
  }

  fprintf(stderr, "rt: rt_val_equals does not support type %i\n", a->type);
  exit(EXIT_FAILURE);
  return false;
}

bool rt_val_truthy(Value a) {
  
  if (a->type == T_NIL) {
    return false;
  }

  if (a->type == T_BOOL) {
    bool* ptr = (bool*) a->data;
    return *ptr;
  }

  if (a->type == T_STR) {
    return true;
  }

  if (a->type == T_INT) {
    return true;
  }

  if (a->type == T_LIST) {
    return true;
  }

  fprintf(stderr, "rt: rt_val_equals does not support type %i\n", a->type);
  exit(EXIT_FAILURE);
  return false;
}

int64_t rt_val_int(Value v) {
  assert_type(v, T_INT);
  int64_t* ptr = (int64_t*) v->data;
  return *ptr;
}

Value rt_val_plus(Runtime rt, Context ctx, Value a, Value b) {
  assert_type(a, T_INT);
  assert_type(b, T_INT);

  int64_t* a_ptr = (int64_t*) a->data;
  int64_t* b_ptr = (int64_t*) b->data;

  int64_t result = *a_ptr + *b_ptr;

  return rt_alloc_int(rt, ctx, result);
}

void rt_println(Value v) {

  if (v->type == T_NIL) {
    printf("nil\n");
    return;
  }

  if (v->type == T_STR) {
    char* str = (char*)v->data;
    printf("%s\n", str);
    return;
  }

  if (v->type == T_INT) {
    int64_t* ival = (int64_t*)v->data;
    printf("%lld\n", *ival);
    return;
  }

  fprintf(stderr, "rt: rt_val_println does not support type %i\n", v->type);
  exit(EXIT_FAILURE);
}
