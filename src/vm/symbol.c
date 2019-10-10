#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include "../errors.h"
#include "internal.h"

/*
 * SymbolTable
 */

#define SYMBOL_TABLE_MIN_ENTRIES 16
#define SYMBOL_TABLE_MIN_LOAD .40
#define SYMBOL_TABLE_MAX_LOAD .70

// TODO: understand why this algorithm works
// http://hg.openjdk.java.net/jdk7u/jdk7u6/jdk/file/8c2c5d63a17e/src/share/classes/java/lang/String.java
uint32_t stringHash(String *s) {
  uint32_t h = s->hash;
  if (h == 0 && s->length > 0) {
    wchar_t *val = stringValue(s);
    for (uint64_t i=0; i<s->length; i++) {
      h = 31 * h + val[i];
    }
    s->hash = h;
  }
  return h;
}

void _tableEntryInitContents(TableEntry *e) {
  e->used = false;
  e->name = W_NIL_VALUE;
  e->nameHash = 0;
  e->value = W_NIL_VALUE;
}

void _tableInitContents(Table *t) {
  t->numAllocatedEntries = 0;
  t->entries = NULL;
  t->size = 0;
}

void tableFreeContents(Table *t) {
  if (t != NULL) {
    free(t->entries);
    t->numAllocatedEntries = 0;
    t->entries = NULL;
    t->size = 0;
  }
}

void tableInit(Table *table) {

  _tableInitContents(table);

  table->numAllocatedEntries = SYMBOL_TABLE_MIN_ENTRIES;
  table->entries = malloc(sizeof(TableEntry) * table->numAllocatedEntries);
  if (table->entries == NULL) {
    explode("failed to allocate TableEntries array");
  }
  for (uint64_t i=0; i<table->numAllocatedEntries; i++) {
    _tableEntryInitContents(&table->entries[i]);
  }
}

TableEntry* findEntry(VM *vm, Table *table, String* name, uint32_t hash) {

  uint64_t index = hash % table->numAllocatedEntries;

  for (uint64_t i = index; i < table->numAllocatedEntries; i++) {
    TableEntry *entry = &table->entries[i];

    if (!entry->used) {
      return entry;
    } else {
      String *thisName = deref(vm, entry->name);
      if (wcscmp(stringValue(name), stringValue(thisName)) == 0) {
        return entry;
      }
    }
  }

  for (uint64_t i = 0; i < index; i++) {
    TableEntry *entry = &table->entries[i];

    if (!entry->used) {
      return entry;
    } else {
      String *thisName = deref(vm, entry->name);
      if (wcscmp(stringValue(name), stringValue(thisName)) == 0) {
        return entry;
      }
    }
  }

  explode("could not find an available TableEntry");
}

Value tableLookup(VM *vm, Table *table, Value name) {

  if (valueType(name) != VT_STR) {
    explode("names must be strings");
  }

  String *s = deref(vm, name);
  uint32_t hash = stringHash(s);

  TableEntry *found = findEntry(vm, table, s, hash);

  if (found->used) {
    return found->value;
  }
  else {
    return W_NIL_VALUE;
  }
}

void _putEntryWithHash(VM *vm, Table *table, Value insertMe, Value name, uint32_t hash) {
  TableEntry *found = findEntry(vm, table, deref(vm, name), hash);
  if (!found ->used) {
    table->size++;
    found->used = true;
  }
  found->name = name;
  found->nameHash = hash;
  found->value = insertMe;
}

void _putEntry(VM *vm, Table *table, Value name, Value insertMe) {
  String *s = deref(vm, name);
  uint32_t hash = stringHash(s);
  _putEntryWithHash(vm, table, insertMe, name, hash);
}

void putEntry(VM *vm, Table *table, Value name, Value insertMe) {

  _putEntry(vm, table, name, insertMe);

  float load = (float)table->size / (float)table->numAllocatedEntries;

  // resize
  if (load > SYMBOL_TABLE_MAX_LOAD || (load > SYMBOL_TABLE_MIN_ENTRIES && load < SYMBOL_TABLE_MIN_LOAD)) {

    uint64_t newAllocatedEntries;
    if (load > SYMBOL_TABLE_MAX_LOAD) {
      newAllocatedEntries = table->numAllocatedEntries * 2;
    }
    else {
      newAllocatedEntries = table->numAllocatedEntries / 2;
      if (newAllocatedEntries < SYMBOL_TABLE_MIN_ENTRIES) {
        newAllocatedEntries = SYMBOL_TABLE_MIN_ENTRIES;
      }
    }

    uint64_t numOldEntries = table->numAllocatedEntries;
    TableEntry *oldEntries = table->entries;

    table->size = 0;
    table->numAllocatedEntries = newAllocatedEntries;
    table->entries = malloc(sizeof(TableEntry) * newAllocatedEntries);
    if (table->entries == NULL) {
      explode("failed to allocate TableEntries array");
    }
    for (uint64_t i=0; i<table->numAllocatedEntries; i++) {
      _tableEntryInitContents(&table->entries[i]);
    }

    for (uint64_t i=0; i<numOldEntries; i++) {
      TableEntry *oldEntry = &oldEntries[i];
      if (oldEntry->used) {
        _putEntryWithHash(vm, table, oldEntry->value, oldEntry->name, oldEntry->nameHash);
      }
    }

    free(oldEntries);
  }
}

