#include <stdlib.h>
#include <stdio.h>
#include "chunk.h"
#include "memory.h"
#include "value.h"
#include "line.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    initLine(&chunk->lines);
    initValueArray(&chunk->constants);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    writeLine(&chunk->lines, line);
    chunk->count++;
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    freeLine(&chunk->lines);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}

void writeConstant(Chunk* chunk, Value value, int line) {
    int index = addConstant(chunk, value);
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, index, line);
}

void writeLongConstant(Chunk* chunk, Value value, int line) {
    int constantIndex = addConstant(chunk, value);
    /*OP_CONSTANT_LONG uses 24bits(3bytes) to store operands
    this allows it to store up to 256*256*256=16,777,216 - 1 different constants
    unlike OP_CONSTANT that uses on 8bits(1byte) - which can only store up to 256 constants.
    Given an index of 100,000, we will convert it to base 256 which is equivalent to [1, 134, 160].
    we will then store these values as the operands of the OP_CONSTANT_LONG instruction.
    */
   if (constantIndex > 16777215) {
        printf("Overflow error: OP_CONSTANT_LONG can only store up to 16,777,215 constants\n");
        exit(1);
   }

   int v3 = constantIndex % 256;
   int v2 = (constantIndex / 256) % 256;
   int v1 = (constantIndex / 256 / 256) % 256;

   writeChunk(chunk, OP_CONSTANT_LONG, line);
   writeChunk(chunk, v1, line);
   writeChunk(chunk, v2, line);
   writeChunk(chunk, v3, line);
}