#pragma once

#include "common.h"
#include "value.h"
#include "line.h"

typedef enum {
    OP_CONSTANT,
    OP_TRUE,
    OP_FALSE,
    OP_NIL,
    OP_CONSTANT_LONG,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MODULO,
    OP_TERNARY,
    OP_NOT,
    OP_AND,
    OP_OR,
    OP_EQUALS,
    OP_GREATER,
    OP_LESS,
    OP_GREATER_EQAULS,
    OP_LESS_EQUALS,
    OP_RETURN,
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    LineArray lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
void freeChunk(Chunk* chunk);
int addConstant(Chunk* Chunk, Value value);
void updateLines(Chunk* chunk, int line);
void writeConstant(Chunk* chunk, Value value, int line);
void writeLongConstant(Chunk* chunk, Value value, int line);