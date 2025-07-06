#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
#define UINT8_COUNT (UINT8_MAX + 1)
#define MAX_LOCAL_VARIABLE (UINT8_COUNT)

typedef enum {
    VAL_BOOL,
    VAL_NUMBER,
    VAL_NIL,
    VAL_OBJ
} ValueType;