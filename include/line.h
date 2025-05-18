#pragma once
#include "common.h"

typedef struct {
    int capacity;
    int count;
    int* lines;
} LineArray;

void initLine(LineArray* array);
void writeLine(LineArray* array, int value);
int getLine(LineArray* array, int index);
void freeLine(LineArray* array);