#include <stdlib.h>
#include <stdio.h>
#include "debug.h"

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}
 
static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constantIndex = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constantIndex);
    printValue(chunk->constants.values[constantIndex]);
    printf("'\n");
    return offset + 2;
}

static int constantLongInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t v1 = chunk->code[offset + 1];
    uint8_t v2 = chunk->code[offset + 2];
    uint8_t v3 = chunk->code[offset + 3];

    int constantIndex = (v1 * 256 * 256) + (v2 * 256) + v3;
    printf("%-16s %4d '", name, constantIndex);
    printValue(chunk->constants.values[constantIndex]);
    printf("'\n");
    return offset + 4;
}

void disassembleChunk(Chunk* chunk, char* name) {
    printf("=== %s ===\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d", offset);
    //show a | for instructions that come S from the same source line.
    if (offset > 0 && getLine(&chunk->lines, offset) == getLine(&chunk->lines, offset-1)) {
        printf("%4s ", "|");
    } else {
        printf("%4d ", getLine(&chunk->lines, offset));
    }

    uint8_t instruction = chunk->code[offset];

    switch (instruction) {
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_CONSTANT_LONG:
            return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
        case OP_TRUE:
            return simpleInstruction("OP_TRUE", offset);
        case OP_FALSE:
            return simpleInstruction("OP_FALSE", offset);
        case OP_NIL:
            return simpleInstruction("OP_NIL", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_ADD: 
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT: 
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY: 
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_MODULO:
            return simpleInstruction("OP_MODULO", offset);
        case OP_NOT:
            return simpleInstruction("OP_NOT", offset);
        case OP_AND:
            return simpleInstruction("OP_AND", offset);
        case OP_OR:
            return simpleInstruction("OP_OR", offset);
        case OP_EQUALS:
            return simpleInstruction("OP_EQUALS", offset);
        case OP_GREATER:
            return simpleInstruction("OP_GREATER", offset);
        case OP_LESS:
            return simpleInstruction("OP_LESS", offset);
        case OP_GREATER_EQAULS:
            return simpleInstruction("OP_GREATER_EQAULS", offset);
        case OP_LESS_EQUALS:
            return simpleInstruction("OP_LESS_EQUALS", offset);
        case OP_DEFINE_GLOBAL:
            return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
        case OP_GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);
        case OP_POP:
            return simpleInstruction("OP_POP", offset);
        case OP_GET_LOCAL:
            return byteInstruction("OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return byteInstruction("OP_SET_LOCAL", chunk, offset);
        case OP_PRINT:
            return simpleInstruction("OP_PRINT", offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}

