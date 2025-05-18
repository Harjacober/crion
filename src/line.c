#include <stdlib.h>
#include "line.h"
#include "memory.h"

void initLine(LineArray* array) {
    array->capacity = 0;
    array->count = 0;
    array->lines = NULL;
}

void writeLine(LineArray* array, int line) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->lines = GROW_ARRAY(int, array->lines, oldCapacity, array->capacity);
    }
   
    int lastIndex = array->count - 1;
    if (lastIndex > 0 && line == array->lines[lastIndex]) {
        array->lines[lastIndex - 1]++;
    } else {
        array->lines[array->count] = 1;
        array->lines[array->count + 1] = line;
        array->count += 2;
    }   
}

int getLine(LineArray* array, int index) {
    int maxIndex = 0;
    for (int i = 0; i < array->count; i = i + 2) {
        maxIndex += array->lines[i];
        if (index < maxIndex) {
            return array->lines[i + 1];
        }
    }
    return -1;
}

void freeLine(LineArray* array) {
    FREE_ARRAY(int, array->lines, array->capacity);
    initLine(array);
}