#include <stdlib.h>
#include <stdio.h>

#include "memory.h"
#include "vm.h"

typedef struct ObjString ObjString;

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    if (newSize == 0){
        free(pointer);
        return NULL;
    }    
    void* result = realloc(pointer, newSize);
    if (result == NULL) {
        exit(1);
    }
    return result;
}

static void freeObject(Obj* object) {
    switch (object->type) {
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        default:
            break;
    }
}

void freeObjects() {
    Obj* curr = vm.objects;
    while (curr != NULL) {
        Obj* next = curr->next;
        freeObject(curr);
        curr = next;
    }
    
}