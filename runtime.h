#include <stdint.h>

typedef enum ValueTypes {
  T_NIL,
  T_BOOL,
  T_STR,
  T_INT,
  T_RECORD,
  T_LIST,
  T_MAP,
  T_FN
} ValueTypes;

typedef uint64_t ValueRef;
