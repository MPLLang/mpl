#include <pthread.h>
#include "chunk-pool.h"
#include <stdlib.h>
#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_FUNCS

int main(){

  struct ChunkPool_config c;
  c.initialSize = 500;
  c.maxSize = 5000000000;
  c.liveRatio = 1.5;
  
  return 0;
}
