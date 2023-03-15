#ifndef WARP_LANG_SYMBOL_H
#define WARP_LANG_SYMBOL_H

#include "vm.h"

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

uint32_t stringHash(String *s);
void tableFreeContents(Table *t);
void tableInit(Table *table);
Value tableLookup(VM_t vm, Table *table, Value name);
void putEntry(VM_t vm, Table *table, Value name, Value insertMe);

#endif //WARP_LANG_SYMBOL_H
