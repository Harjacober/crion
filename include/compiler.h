#pragma once
#include "chunk.h"
#include "scanner.h"
#include "vm.h"

bool compile(Chunk* chunk, const char* source);
