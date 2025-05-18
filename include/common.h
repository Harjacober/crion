#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION

typedef enum {
    VAL_BOOL,
    VAL_NUMBER,
    VAL_NIL
} ValueType;