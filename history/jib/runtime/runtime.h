#include <stdbool.h>
#include <stdint.h>

/**
 * Defines an opaque pointer to a Runtime instance. The rt_* functions describe
 * how to interact with this. This abstraction encapsulates all behavior around
 * allocating, garbage collecting, and manipulating primitive language
 * constructs.
 */
typedef struct s_Runtime *Runtime;

/**
 * Create an instance of a Runtime.
 * 
 * @param max_stack_addr - The highest address in the stack to consider when
 * building the root set of objects for garbage collection. This should probably
 * just be address of the base pointer for the *main* stack frame.
 *
 * @param max_heap_size - The maximum memory that can be allocated to the
 * garbage collected objects on the heap.
 *
 * @param err - If this function encounters an error it will set the referenced
 * integer to a non-zero value. Specific error codes are not defined.
 *
 * @returns an opaque pointer to a Runtime instance.
 */
Runtime rt_create(void* max_stack_addr, uint64_t max_heap_size);

/**
 * Destroy an instance of a Runtime.
 *
 * @param rt - An opaque pointer to a Runtime instance.
 * 
 * @param err - If this function encounters an error it will set the referenced
 * integer to a non-zero value. Specific error codes are not defined.
 */
void rt_destroy(Runtime rt);

typedef struct s_Context {
  void* base_pointer;
  void* stack_pointer;
} Context;

Context rt_create_parent_frame_context();

/**
 * An instance of a primitive data structure. Nil, Strings, Ints and Cons cells
 * are currently supported. Represented as an opaque pointer. Entirely provided
 * by the Runtime abstraction.
 */
typedef struct s_Value *Value;

// allocation
Value rt_alloc_nil(Runtime rt, Context ctx);
Value rt_alloc_bool(Runtime rt, Context ctx, bool b);
Value rt_alloc_str(Runtime rt, Context ctx, char* s);
Value rt_alloc_int(Runtime rt, Context ctx, int64_t i);
Value rt_alloc_list(Runtime rt, Context ctx);
Value rt_alloc_fn(Runtime rt, Context ctx, void* address_ptr);

// introspection
typedef enum s_Type {
  T_NIL,
  T_BOOL,
  T_STR,
  T_INT,
  T_CONS,
  T_LIST,
  T_FN
} Type;

Type rt_val_type(Value v);
uint64_t rt_val_size(Value v);

void* rt_val_fn_ptr(Value fn);

// language builtins
Value rt_val_cons(Runtime rt, Context ctx, Value head, Value tail);
Value rt_val_first(Value v);
Value rt_val_rest(Runtime rt, Context ctx, Value v);
Value rt_val_count(Runtime rt, Context ctx, Value v);
bool rt_val_equals(Value a, Value b);
bool rt_val_truthy(Value v);
int64_t rt_val_int(Value v);
Value rt_val_plus(Runtime rt, Context ctx, Value a, Value b);

// io
void rt_println(Value v);
