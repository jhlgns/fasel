#pragma once

#include "memory_pool.h"

struct AstNode;

AstNode *desugar(MemoryPool &pool, AstNode *ast);
