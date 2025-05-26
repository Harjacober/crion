#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "memory.h"
#include "object.h"
#include "value.h"

#define LOAD_FACTOR 0.75

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;
    for (; ;) {
        Entry* entry = &entries[index];
         if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) {
                    tombstone = entry;
                }
            }
        } else if (entry->key == key) {
            return entry;
        }
        
        index = (index + 1) % capacity;
    } 
}

static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    table->count = 0;

    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    for (int i = 0; i < table->capacity; i++) {
        Entry* oldEntry = &table->entries[i];
    
        if (oldEntry->key == NULL) {
            continue;
        }

        Entry* newEntry = findEntry(entries, capacity, oldEntry->key);
        newEntry->key = oldEntry->key;
        newEntry->value = oldEntry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->capacity = capacity;
    table->entries = entries;
}

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
} 

bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * LOAD_FACTOR) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey =  entry->key == NULL;
    if (isNewKey && IS_NIL(entry->value)) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;
    
    return isNewKey;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* fromEntries = &from->entries;
        if (fromEntries->key != NULL) {
            tableSet(to, fromEntries->key, fromEntries->value);
        }
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) {
        return false;
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) {
        return false;
    }

    *value = entry->value;
    return true;
}

bool tableDelete(Table* table, ObjString* key) {
    Entry* entry = findEntry(table->entries, table->capacity, key);

    if (entry->key != NULL) {
        entry->key = NULL;
        entry->value = TO_BOOL_VAL(true);
        return true;
    }

    return false;
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) {
        return NULL;
    }
    uint32_t index = hash % table->capacity;
    for (; ;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                return NULL;
            }
        } else {
            if (entry->key->length == length && entry->key->hash == hash && 
            memcmp(entry->key->chars, chars, length) == 0) {
                return entry->key;
            }
        }

        index = (index + 1) % table->capacity;
    }
}