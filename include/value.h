#pragma once
#include "common.h"


typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
    } as;
    
} Value;

#define TO_BOOL_VAL(value)      ((Value) {VAL_BOOL, {.boolean = value}})
#define TO_NUMBER_VAL(value)    ((Value) {VAL_NUMBER, {.number = value}})
#define TO_NIL_VAL              ((Value) {VAL_NIL, {.number = 0}})

#define AS_C_BOOL(value)      ((value).as.boolean)
#define AS_C_NUMBER(value)    ((value).as.number)

#define IS_BOOL(value)     ((value).type == VAL_BOOL)
#define IS_NUMBER(value)     ((value).type == VAL_NUMBER)
#define IS_NIL(value)     ((value).type == VAL_NIL)

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);
