#pragma once
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value))->type
#define IS_STRING(value)    isObjType(value, OBJ_STRING)
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   ((ObjString*)AS_OBJ(value))->chars

typedef enum {
    OBJ_STRING
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next;
};

struct ObjString {
    Obj obj;
    int length;
    int hash;
    char* chars;
};

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && OBJ_TYPE(value) == type;
}

ObjString* copyString(const char* chars, int length);

ObjString* takeString(const char* chars, int length);
