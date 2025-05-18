#pragma once

#include "chunk.h"

void disassembleChunk(Chunk* chunk, char* name);
int disassembleInstruction(Chunk* Chunk, int offset);