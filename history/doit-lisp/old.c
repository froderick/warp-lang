#include <stdio.h>
#include <stdlib.h>
#include "config.h"

/*
  concurrent mark-and-sweep:

  - maintain a linked-list of objects that have been allocated
  - each object is one of the following:
    1. an atom (cannot reference other things)
    2. a list (can reference other things)
    3. a function (can close over bindings and vars)

  - when new objects are created, allocate memory as needed, create the objects, and push them onto the list

  - when you want to create a new object but memory is low, force a GC before allocating a new object

  - on forced GC:
    a. traverse all reachable objects (starting from stack and global vars), mark all reached objects
    b. iterate over all allocated objects in list
       1. free any that aren't reachable
       2. clear reachable bit for the rest
 */

// primitives

typedef enum { false, true } bool;

typedef struct Str {
  int len;
  char* data;
} Str;

typedef Str Symbol;
typedef Str Keyword;

typedef struct Cell {
  struct Object* car;
  struct Object* cdr;
} Cell;

typedef enum {T_NIL, T_BOOL, T_INT, T_STR, T_SYMBOL, T_KEYWORD, T_CELL} Type;

typedef struct Object {
  Type type;
  bool marked;
  union {
    bool bool;
    int integer;
    Str str;
    Symbol symbol;
    Keyword keyword;
    Cell cell;
  };
} Object;

// memory management

typedef struct Node {
  Object* obj;
  struct Node* next;
} Node;

Node* root;

void addObject(Object* obj) {
  if (root == NULL) {
    root = malloc(sizeof(Node));
    root->obj = obj;
    root->next = NULL;
  }
  else {
    Node* next = root;
    root = malloc(sizeof(Node));
    root->obj = obj;
    root->next = next;
  }
}

int countObjects() {
  Node* node = root;
  int count = 0;
  while (node != NULL) {
    count++;
    node = node->next;
  }
  return count;
}

Object* makeString(char* s, int len) {
  Object* obj = malloc(sizeof(Object));
  obj->type = T_STR;
  obj->marked = false;
  obj->str.data = s;
  obj->str.len = len;
  return obj;
}

int main(int argv, char** argc) {
  printf("pushing onto the list: \n");

  for (int i=0; i<100; i++) {
    Object* obj = makeString("hi there", sizeof("hi there"));
    addObject(obj);
  }

  printf("list size: %i\n", countObjects());

  return 0;
}
