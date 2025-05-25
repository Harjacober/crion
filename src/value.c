#include <stdlib.h>
#include <stdio.h>
#include "value.h"
#include "memory.h"
#include "object.h"

void initValueArray(ValueArray* array) {
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)){
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        default:
            break;
    }
}

void printValue(Value value) {
    int type = value.type;
    switch (type) {
        case VAL_NUMBER:
            printf("%g", AS_C_NUMBER(value));
            break;
        case VAL_BOOL:
            printf("%s", AS_C_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_OBJ:
            printObject(value);
            break;
    }
}