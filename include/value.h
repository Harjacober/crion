#pragma once

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;
    
} Value;

#define TO_BOOL_VAL(value)      ((Value) {VAL_BOOL, {.boolean = value}})
#define TO_NUMBER_VAL(value)    ((Value) {VAL_NUMBER, {.number = value}})
#define TO_NIL_VAL              ((Value) {VAL_NIL, {.number = 0}})
#define TO_OBJ_VAL(object)      ((Value) {VAL_OBJ, {.obj = object}})

#define AS_C_BOOL(value)      ((value).as.boolean)
#define AS_C_NUMBER(value)    ((value).as.number)
#define AS_OBJ(value)         ((value).as.obj)  

#define IS_BOOL(value)      ((value).type == VAL_BOOL)
#define IS_NUMBER(value)    ((value).type == VAL_NUMBER)
#define IS_NIL(value)       ((value).type == VAL_NIL)
#define IS_OBJ(value)       ((value).type == VAL_OBJ)

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);
